package lms.ir.opt

import scala.collection.mutable

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast
import lms.ir
import lms.util.{Counter, SourceContext}
import lms.util.collection.*
import lms.runtime.Log

import EGraph.EClass
import Stmt.*

object Builder {
  case class Config(rules: Seq[Rule], cfg: EGraph.Config)
  enum Handle {
    case Global(name: String)
    case Local(cls: EGraph.EClass, effects: Seq[Stmt])
  }
}

private class RegionStack {
  private val base: mutable.ArrayBuffer[Stmt] = mutable.ArrayBuffer()
  private val stack: mutable.Stack[mutable.ArrayBuffer[Stmt]] = mutable.Stack()

  private def top: mutable.ArrayBuffer[Stmt] = stack.peek.getOrElse(base)

  def openRegion(): Unit = stack.push(mutable.ArrayBuffer())
  def closeRegion(): Seq[Stmt] = stack.popSafe().map(_.toSeq).getOrElse {
    Log.error("BUG: attempted to `closeRegion` with empty region stack")
    val result = base.toSeq
    base.clear()
    result
  }

  def ret(cls: EClass): EClass = {
    top.append(Return(cls))
    cls
  }
}

private class FunctionBuilder(
    rules: Seq[Rule],
    config: EGraph.Config,
    predefs: Set[String]
) {
  import Builder.Handle.*

  private val counter = Counter()
  private val graph = EGraph(rules, config)
  private val env = mutable.Map.from(predefs.map { name =>
    name -> graph.addNamedVar(name)
  })
  private val regions = RegionStack()

  def register(name: String): EClass = {
    val result = graph.addNamedVar(name)
    env(name) = result
    result
  }

  object Handle {
    def unapply(handle: Builder.Handle): Option[(EClass, Seq[Stmt])] = handle match {
      case Local(cls, effects) => Some((cls, effects))
      case Global(name) => Some((ensureClass(name), Nil))
    }

    def ensureClass(handle: Builder.Handle): EClass = unapply(handle).get._1

    def ensureClass(name: String): EClass =
      env.get(name).getOrElse {
        Log.warning(s"BUG: unregistered global $name was used before it was declared")
        register(name)
      }
  }

  def ret(handle: Builder.Handle): EClass = regions.ret(Handle.ensureClass(handle))

  def beginRegion(): Unit = regions.openRegion()
  def endRegion(): Seq[Stmt] = regions.closeRegion()

  def extract(handle: Builder.Handle): ast.Function = {
    ret(handle)
    extractImpl()
  }

  def extractImpl(): ast.Function = {}
}

class Builder(cfg: Builder.Config) extends ir.Builder {
  type Exp = Builder.Handle
  type Name = String

  import Builder.Handle.*

  private val counter = Counter()

  private val builtins = mutable.Set[String]()
  private var functions = mutable.Map[String, ast.Function]()

  private var current: Option[FunctionBuilder] = None

  def fresh(): Name = s"x${counter.tick()}"
  def name(s: String): Name = s

  def predefs(): Set[String] = builtins.toSet ++ functions.keySet

  def variable(name: Name): Exp = current match {
    case None => {
      builtins.add(name)
      Global(name)
    }
    case Some(ctx) => Local(ctx.register(name), Seq())
  }

  private def topfun(
      mname: Option[String],
      args: Seq[(Name, Type)],
      outty: Type,
      body: => Exp
  ): Exp = {
    val name = this.name(mname.getOrElse { fresh() })

    val builder = FunctionBuilder(cfg.rules, cfg.cfg, predefs())
    current = Some(builder)
    val tail = body
    val result = builder.extract(tail)
    current = None

    Global(name)
  }

  private def ensureBuilder(msg: String)(using s: SourceContext): FunctionBuilder =
    current.getOrElse {
      Log.error(s"BUG: $msg")(using s)
      throw RuntimeException(s"BUG: $msg")
    }

  def fun(name: Option[String], top: Boolean, args: Seq[(Name, Type)], outty: Type)(
      body: => Exp
  ): Exp = if top then topfun(name, args, outty, body) else { }

  def lift[A: Liftable](x: A): Exp = reflect(Op.Const(x), Nil)

  def reflect(op: Op, children: Seq[Exp]): Exp = {}

  // CR-someday cwong: We should have this function take the SourceContext of
  // its caller for the logs.
  def region(f: => Exp): Exp = {
    val ctx = ensureBuilder("attempted to `region` outside function")
    ctx.beginRegion()
    val tail = f
    val result = ctx.ret(tail)
    val block = ctx.endRegion()
    Local(result, block)
  }

  def extract(): ast.Program = ast.Program(functions.toSeq)
}
