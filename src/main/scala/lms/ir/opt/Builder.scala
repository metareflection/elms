package lms.ir.opt

import scala.collection.mutable

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast
import lms.ir
import lms.util.{Counter, SourceContext}
import lms.util.Plumbing.*
import lms.util.collection.*
import lms.runtime.*

import EGraph.EClass
import Stmt.*

object Builder {
  case class Config(rules: Seq[Rule], cfg: EGraph.Config)
  enum Handle {
    case Global(name: String)
    case Local(cls: EClass)
    // The EClass here is solely so downstream users can register nodes in the
    // EGraph; think of it as `v <- body`
    //
    // CR cwong: This representation is very scary; we're likely to drop effects
    // if we never actually write the body back into the CFG.
    case Region(v: String, cls: EClass, body: Stmt)
  }
}

private class RegionBuilder(fresh: () => String) {
  val body: mutable.ArrayBuffer[(String, EClass, Stmt)] = mutable.ArrayBuffer()
  var tail: Option[EClass] = None

  def clear(): Unit = {
    tail = None
    body.clear()
  }

  def push(name: String, cls: EClass, stmt: Stmt): Unit = {
    body += ((name, cls, stmt))
  }

  def ret(cls: EClass): Unit = { tail = Some(cls) }

  def extract(): Stmt = {
    val last = tail.getOrElse {
      throw LMSRuntimeException(
        "BUG: attempted to `RegionBuilder.finalize` with no `ret`"
      )
    }

    body.foldRight(Return(last)) { case ((name, _, stmt), acc) => Let(name, stmt, acc) }
  }
}

private class RegionStack(fresh: () => String) {
  private val base: RegionBuilder = RegionBuilder(fresh)
  private val stack: mutable.Stack[RegionBuilder] = mutable.Stack()

  private def top: RegionBuilder = stack.peek.getOrElse(base)

  def openRegion(): Unit = stack.push(RegionBuilder(fresh))
  def closeRegion(): Stmt = stack.popSafe().map(_.extract()).getOrElse {
    Log.error("BUG: attempted to `closeRegion` with empty region stack")
    base.extract()
  }

  def push(name: String, cls: EClass, stmt: Stmt): Unit = top.push(name, cls, stmt)

  def ret(cls: EClass): Unit = top.ret(cls)

  def isEmpty: Boolean = stack.isEmpty

  def extract(): Stmt = base.extract()
}

private class FunctionBuilder(
    rules: Seq[Rule],
    config: EGraph.Config,
    predefs: Set[String],
    fresh: () => String
) {
  import Builder.Handle.*

  private val counter = Counter()
  private val graph = EGraph(rules, config)
  private val env = Map.from(predefs.map { name => name -> graph.addNamedVar(name) })
  private val regions = RegionStack(fresh)

  def register(name: String): EClass = graph.addNamedVar(name)

  def ensureClass(name: String): EClass = env.get(name).getOrElse {
    Log.warning(s"BUG: unregistered name $name was used before it was declared")
    register(name)
  }

  extension (handle: Builder.Handle)
    def unwrap: EClass = handle match {
      case Local(cls)              => cls
      case Global(name)            => ensureClass(name)
      case Region(name, cls, body) => {
        regions.push(name, cls, body)
        cls
      }
    }

    def asStmt: Stmt = handle match {
      case Local(cls)              => Return(cls)
      case Global(name)            => Return(ensureClass(name))
      case Region(name, cls, body) => body
    }

  private object Handle {
    def unapply(handle: Builder.Handle): Option[EClass] = Some(handle.unwrap)
  }

  def reflect(op: Op, children: Seq[Builder.Handle]): Builder.Handle = op match {
    case pure: Op.Pure     => reflectPure(pure, children.map(_.unwrap))
    case eff: Op.Effectful => reflectEffect(eff, children.map(_.unwrap))
    case ctrl: Op.Control  => reflectControl(ctrl, children)
  }

  private def reflectPure(op: Op.Pure, children: Seq[EClass]): Builder.Handle =
    Local(graph.addNode(op, children))

  private def reflectEffect(op: Op.Effectful, children: Seq[EClass]): Builder.Handle = {
    val name = fresh()
    val cls = graph.addNamedVar(name)
    regions.push(name, cls, Effect(op, children))
    Local(cls)
  }

  private def reflectControl(
      op: Op.Control,
      children: Seq[Builder.Handle]
  ): Builder.Handle = {
    val name = fresh()
    val cls = graph.addNamedVar(name)

    op match {
      case Op.IfThenElse => children match {
          case Seq(guard, thn, els) => regions
              .push(name, cls, If(guard.unwrap, thn.asStmt, els.asStmt))
          case _ => throw LMSRuntimeException("BUG: IfThenElse invalid children")
        }
      case Op.RangeForEach => children match {
          case Seq(x, st, end, body) => regions
              .push(name, cls, RangeFor(???, st.unwrap, end.unwrap, asStmt(body)))
          case _ => throw LMSRuntimeException("BUG: RangeForEach invalid children")
        }
    }
    Local(cls)
  }

  def ret(handle: Builder.Handle): Unit = regions.ret(handle.unwrap)

  def openRegion(): Unit = regions.openRegion()
  def closeRegion(): (String, EClass, Stmt) = {
    val stmt = regions.closeRegion()
    val name = fresh()
    val cls = graph.addNamedVar(name)
    (name, cls, stmt)
  }

  def extract: ast.Term = {
    if !regions.isEmpty then {
      Log.warning("BUG: attempted to `extract` without closing all regions")
    }

    elab(regions.extract(), ScopeMap())
  }

  class ScopeMap(
      parent: Option[ScopeMap] = None,
      vs: mutable.Map[EClass, String] = mutable.Map()
  ) {
    def get(cls: EClass): Option[String] = vs.get(cls)
      .orElse { parent.flatMap { _.get(cls) } }

    def update(cls: EClass, name: String): Unit = { vs(cls) = name }
  }

  def elab(s: Stmt, cache: ScopeMap): ast.Term = {
    val (prefix, tail) = elabImpl(s, cache)
    prefix.foldRight(tail) { case ((x, e), acc) => ast.Let(x, e, acc) }
  }

  def elabCls(cls: EClass, cache: ScopeMap): (Seq[(String, ast.Term)], ast.Term) =
    cache.get(cls) match {
      case Some(v) => (Seq(), ast.V(v))
      case None    => {
        val name = fresh()
        val result = graph.extract(cls)
          .getOrElse { throw LMSRuntimeException(s"BUG: invalid EClass $cls") }
        cache(cls) = name
        (Seq((name, result)), ast.V(name))
      }
    }

  def elabImpl(s: Stmt, cache: ScopeMap): (Seq[(String, ast.Term)], ast.Term) =
    s match {
      case Return(cls)    => elabCls(cls, cache)
      case Let(x, e1, e2) => {
        val (prefix1, t1) = elabImpl(e1, cache)
        val (prefix2, t2) = elabImpl(e2, cache)
        ((prefix1 :+ (x, t1)) ++ prefix2, t2)
      }
      case Effect(op, children) => {
        children.map(elabCls(_, cache))
          .foldLeft(Vector.empty[(String, (ast.Term))], Vector.empty[ast.Term]) {
            case ((prefixAcc, terms), (prefix, term)) =>
              (prefixAcc ++ prefix, terms :+ term)
          }.mapRight(ast.E(op, _))
      }
      case If(cond, thn, els)         => ???
      case RangeFor(v, st, end, body) => ???
    }
}

class Builder(cfg: Builder.Config) extends ir.Builder {
  type Exp = Builder.Handle
  type Name = String

  import Builder.Handle.*

  private val counter = Counter()

  private val builtins = mutable.Set[String]()
  private val functions = mutable.Map[String, ast.Function]()

  private var current: Option[FunctionBuilder] = None

  def fresh(): Name = s"x${counter.tick()}"
  def name(s: String): Name = s

  def predefs(): Set[String] = builtins.toSet ++ functions.keySet

  def variable(name: Name): Exp = current match {
    case None => {
      builtins.add(name)
      Global(name)
    }
    case Some(ctx) => Local(ctx.register(name))
  }

  private def topfun(
      mname: Option[String],
      args: Seq[(Name, Type)],
      outty: Type,
      body: => Exp
  ): Exp = {
    val name = this.name(mname.getOrElse { fresh() })

    val builder = FunctionBuilder(cfg.rules, cfg.cfg, predefs(), this.fresh)
    current = Some(builder)
    val tail = body
    builder.ret(tail)
    val result = builder.extract
    current = None
    functions(name) = ast.Function(args, outty, result)
    Global(name)
  }

  private def ensureBuilder(msg: String): FunctionBuilder = current
    .getOrElse { throw LMSRuntimeException(s"BUG: $msg") }

  def fun(name: Option[String], top: Boolean, args: Seq[(Name, Type)], outty: Type)(
      body: => Exp
  ): Exp = if top then topfun(name, args, outty, body) else { ??? }

  def lift[A: Liftable](x: A): Exp = reflect(Op.Const(x), Nil)

  def reflect(op: Op, children: Seq[Exp]): Exp =
    ensureBuilder("attempted to `reflect` outside function").reflect(op, children)

  def region(f: => Exp): Exp = {
    val ctx = ensureBuilder("attempted to `region` outside function")
    ctx.openRegion()
    val tail = f
    ctx.ret(tail)
    val (name, cls, body) = ctx.closeRegion()
    Region(name, cls, body)
  }

  def extract(): ast.Program = ast.Program(functions.toSeq)
}
