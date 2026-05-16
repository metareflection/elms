package lms.pipeline.simple

// A bog-standard builder producing an IR in ANF form according to Rompf '16.

import lms.core.{Type, Op, Name}, Op._
import lms.pipeline
import lms.core.tree as ast
import lms.runtime.Log
import lms.util.{Plumbing, Counter}

class Builder extends pipeline.Builder {
  type Exp = ast.Term

  var roots: List[(Name, ast.Function)] = Nil
  var stBlock: List[(Name, Exp)] = Nil

  import ast._

  def variable(name: Name): Exp = V(name)

  def collect(tail: => Exp): Exp = {
    stBlock = Nil
    val last = tail
    // uncurry . flip Let
    stBlock.foldLeft(last) { case (e2, (name, e1)) => Let(name, e1, e2) }
  }

  override def fun(
      name: Name,
      top: Boolean,
      args: Seq[(Name, Type)],
      outty: Type
  ): FunctionStub = {
    def fill(body: => Exp): Unit = {
      if top then stBlock = Nil

      val bodyexp = region(body)
      val f = ast.Function(args, outty, bodyexp)

      if top then roots ::= (name, f) else stBlock ::= (name, f)
    }

    FunctionStub(
      variable(name), fill
    )
  }

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
