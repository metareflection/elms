package elms.pipeline.eqsat

import scala.collection.mutable

import elms.core.{Type, Op, Name}
import elms.core.tree as ast
import elms.pipeline
import elms.util.{Counter, SourceContext}
import elms.util.Plumbing.*
import elms.util.collection.*
import elms.runtime.*

import EGraph.EClass
import Stmt.*

object Builder {
  case class Config(rules: Seq[Rule], cfg: EGraph.Config = EGraph.Config())
  enum Handle {
    case Global(name: Name)
    case Local(cls: EClass)
    // The EClass here is solely so downstream users can register nodes in the
    // EGraph; think of it as `v <- body`
    //
    // CR cwong: This representation is very scary; we're likely to drop effects
    // if we never actually write the body back into the CFG.
    case Region(v: Name, cls: EClass, body: Stmt)
  }
}

private class RegionBuilder(fresh: () => Name) {
  val body: mutable.ArrayBuffer[(Name, EClass, Stmt)] = mutable.ArrayBuffer()
  var tail: Option[EClass] = None

  def clear(): Unit = {
    tail = None
    body.clear()
  }

  def push(name: Name, cls: EClass, stmt: Stmt): Unit = { body += ((name, cls, stmt)) }

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

private class RegionStack(fresh: () => Name) {
  private val base: RegionBuilder = RegionBuilder(fresh)
  private val stack: mutable.Stack[RegionBuilder] = mutable.Stack()

  private def top: RegionBuilder = stack.peek.getOrElse(base)

  def openRegion(): Unit = stack.push(RegionBuilder(fresh))
  def closeRegion(): Stmt = stack.popSafe().map(_.extract()).getOrElse {
    Log.error("BUG: attempted to `closeRegion` with empty region stack")
    base.extract()
  }

  def push(name: Name, cls: EClass, stmt: Stmt): Unit = top.push(name, cls, stmt)

  def ret(cls: EClass): Unit = top.ret(cls)

  def isEmpty: Boolean = stack.isEmpty

  def extract(): Stmt = base.extract()
}

private class FunctionBuilder(
    name: Name,
    rules: Seq[Rule],
    config: EGraph.Config,
    predefs: Set[Name],
    fresh: () => Name
) {
  import Builder.Handle.*

  private val counter = Counter()
  private val graph = EGraph(Ruleset(rules), config)
  private val env = Map.from((predefs + name).map { name =>
    name -> graph.addNamedVar(name)
  })
  private val regions = RegionStack(fresh)

  def register(name: Name): EClass = graph.addNamedVar(name)

  def ensureClass(name: Name): EClass = env.get(name).getOrElse {
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

  def symbol(name: Name): EClass = graph.addNamedVar(name)

  def lambda(
      name: Name,
      cls: EClass,
      args: Seq[(Name, Type)],
      outty: Type,
      body: Builder.Handle
  ): Unit = regions.push(name, cls, Lambda(args, outty, body.asStmt))

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
      case Op.RangeForEach(x) => children match {
          case Seq(st, end, body) => regions
              .push(name, cls, RangeFor(x, st.unwrap, end.unwrap, body.asStmt))
          case _ => throw LMSRuntimeException("BUG: RangeForEach invalid children")
        }
      case Op.While => children match {
        case Seq(cond, body) => regions.push(name, cls, While(cond.asStmt, body.asStmt))
      }
    }
    Local(cls)
  }

  def ret(handle: Builder.Handle): Unit = regions.ret(handle.unwrap)

  def openRegion(): Unit = regions.openRegion()
  def closeRegion(): (Name, EClass, Stmt) = {
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
      vs: mutable.Map[EClass, Name] = mutable.Map()
  ) {
    def get(cls: EClass): Option[Name] = vs.get(cls)
      .orElse { parent.flatMap { _.get(cls) } }

    def update(cls: EClass, name: Name): Unit = { vs(cls) = name }

    def enter: ScopeMap = ScopeMap(Some(this))
  }

  def elab(s: Stmt, cache: ScopeMap): ast.Term = {
    val (prefix, tail) = elabImpl(s, cache)
    prefix.foldRight(tail) { case ((x, e), acc) => ast.Let(x, e, acc) }
  }

  private type ElabOut = (Seq[(Name, ast.Term)], ast.Term)

  def elabCls(cls: EClass, cache: ScopeMap): ElabOut = cache.get(cls) match {
    case Some(v) => (Seq(), ast.V(v))
    case None    => {
      val name = fresh()
      val result = graph.extract(cls)
        .getOrElse { throw LMSRuntimeException(s"BUG: invalid EClass $cls") }
      cache(cls) = name
      (Seq((name, result)), ast.V(name))
    }
  }

  def elabImpl(s: Stmt, cache: ScopeMap): ElabOut = s match {
    case Return(cls)    => elabCls(cls, cache)
    case Let(x, e1, e2) => {
      val (prefix1, t1) = elabImpl(e1, cache)
      val (prefix2, t2) = elabImpl(e2, cache)
      ((prefix1 :+ (x, t1)) ++ prefix2, t2)
    }
    case Effect(op, children) => {
      children.map(elabCls(_, cache))
        .foldLeft(Vector.empty[(Name, (ast.Term))], Vector.empty[ast.Term]) {
          case ((prefixAcc, terms), (prefix, term)) =>
            (prefixAcc ++ prefix, terms :+ term)
        }.mapRight(ast.E(op, _))
    }
    case If(cond, thn, els) => {
      val (prefix, c) = elabCls(cond, cache)
      val t = elab(thn, cache.enter)
      val e = elab(els, cache.enter)
      (prefix, ast.E(Op.IfThenElse, Seq(c, t, e)))
    }
    case Lambda(args, outty, body) =>
      (Seq(), ast.Function(args, outty, elab(body, cache.enter)))
    case RangeFor(x, st, end, body) => {
      val (prefix1, stt) = elabCls(st, cache)
      val (prefix2, endt) = elabCls(end, cache)
      (
        prefix1 ++ prefix2,
        ast.E(Op.RangeForEach(x), Seq(stt, endt, elab(body, cache.enter)))
      )
    }
    case While(cond, body) => {
      val guardt = elab(cond, cache)
      val bodyt = elab(body, cache.enter)
      (
        Seq(),
        ast.E(Op.While, Seq(guardt, bodyt))
      )
    }
  }
}

class Builder(cfg: Builder.Config) extends pipeline.Builder {
  type Exp = Builder.Handle

  import Builder.Handle.*

  private enum FEntry derives CanEqual {
    case Stub
    case F(func: ast.Function)
  }
  import FEntry.*

  private val builtins = mutable.Set[Name]()
  private val functions = mutable.Map[Name, FEntry]()

  private var current: mutable.Stack[FunctionBuilder] = mutable.Stack()

  def predefs(): Set[Name] = builtins.toSet ++ functions.keySet

  def variable(name: Name): Exp = current.peek match {
    case None => {
      builtins.add(name)
      Global(name)
    }
    case Some(ctx) => Local(ctx.register(name))
  }

  private def ensureBuilder(msg: String): FunctionBuilder = current.peek
    .getOrElse { throw LMSRuntimeException(s"BUG: $msg") }

  private def topfun(name: Name, args: Seq[(Name, Type)], outty: Type): FunctionStub = {
    def fill(body: => Exp): Unit = {
      val builder = FunctionBuilder(name, cfg.rules, cfg.cfg, predefs(), this.fresh)
      current.push(builder)
      val tail = body
      builder.ret(tail)
      val result = builder.extract
      current.pop()
      functions(name) = F(ast.Function(args, outty, result))
    }
    functions(name) = Stub
    FunctionStub(Global(name), fill)
  }

  private def lambda(name: Name, args: Seq[(Name, Type)], outty: Type): FunctionStub = {
    val builder = ensureBuilder("attempted to define lambda outside function")
    val cls = builder.symbol(name)
    def fill(body: => Exp): Unit = builder.lambda(name, cls, args, outty, region(body))

    FunctionStub(Local(cls), fill)
  }

  def fun(
      name: Name,
      top: Boolean,
      args: Seq[(Name, Type)],
      outty: Type
  ): FunctionStub = if top then topfun(name, args, outty) else lambda(name, args, outty)

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

  def extract(): ast.Program = {
    val funcs = functions.toSeq.filterMap {
      case (name, F(func)) => Some((name, func))
      case (_, Stub)       => {
        Log.warning(s"BUG: attempted to `extract` with function $name still stubbed")
        None
      }
    }
    ast.Program(funcs, staticData.toSeq)
  }
}
