package lms.ir.simple

// A bog-standard builder producing an IR in ANF form according to Rompf '16.

import lms.core.{Liftable, Type, Op}, Op._
import lms.ir
import lms.ir.Name
import lms.runtime.Log
import lms.codegen.ast, ast._
import lms.util.{Plumbing, Counter}
import lms.util.typeclasses.given

class Builder extends ir.Builder {
  type Exp = ast.Term

  var roots: List[(Name, ast.Function)] = Nil
  var stBlock: List[(Name, Exp)] = Nil

  def variable(name: Name): Exp = V(name)

  def collect(tail: => Exp): Exp = {
    stBlock = Nil
    val last = tail
    // uncurry . flip Let
    stBlock.foldLeft(last) { case (e2, (name, e1)) => Let(name, e1, e2) }
  }

  override def fun(
      mname: Option[Name],
      top: Boolean,
      args: Seq[(Name, Type)],
      outty: Type
  )(body: => Exp): Exp = {
    val name = mname match {
      case Some(s) => s
      case None    => fresh()
    }

    if top then stBlock = Nil

    val bodyexp = region(body)
    val f = ast.Function(args, outty, bodyexp)

    if top then roots ::= (name, f) else stBlock ::= (name, f)

    variable(name)
  }

  def lift[A: Liftable](x: A): Exp = ast.E(Const(x), Nil)

  def reflect(op: Op, children: Seq[Exp]): Exp = {
    val name = fresh()
    stBlock ::= (name, ast.E(op, children))
    variable(name)
  }

  def region(f: => Exp): Exp = {
    val prev = stBlock
    val result = collect(f)
    stBlock = prev
    result
  }

  def extract(): ast.Program = {
    if (!stBlock.isEmpty) {
      Log.warning("BUG: attempted to `extract` with non-empty `stBlock`")
    }

    ast.Program(roots)
  }
}
