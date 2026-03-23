package lms.ir.anf

import lms.util.Plumbing
import lms.core.{Liftable, Type, Op}, Op._
import lms.ir, ir.Dialect
import lms.runtime.Log
import lms.codegen.ast, ast._

object Anf extends Dialect {
  type Name = Int

  type Exp = ast.Term

  var counter: Int = 0
  var roots: List[(Name, ast.Function)] = Nil
  var stBlock: List[(Name, Exp)] = Nil

  def init(): Unit = {
    counter = 0
    roots = Nil
  }

  def fresh(): Name = {
    val result = counter
    counter += 1
    result
  }

  def variable(name: Name): Exp = V(renderName(name))

  def collect(tail: => Exp): Exp = {
    stBlock = Nil
    val last = tail
    // uncurry . flip Let
    stBlock.foldLeft(last) { case (e2, (name, e1)) =>
      Let(renderName(name), e1, e2)
    }
  }

  def fun(top: Boolean, args: Seq[(Name, Type)], outty: Type)(body: => Exp): Exp = {
    val name = fresh()

    if (top) then stBlock = Nil

    val bodyexp = region(body)
    val f = ast.Function(args.map(Plumbing.onLeft(renderName)), outty, bodyexp)

    if (top) then roots ::= (name, f)
    else stBlock ::= (name, f)

    variable(name)
  }

  def lift[A: Liftable](x: A): Exp =
    ast.E(Const(summon[Liftable[A]].identity)(x), Nil)

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

  def renderName(n: Name): String = s"x$n"

  def extract(): ast.Program = {
    if (!stBlock.isEmpty) {
      val x: Unit =
        Log.warning("INTERNAL BUG: attempted to `extract` with non-empty `stBlock`")
    }

    ast.Program(roots.map(Plumbing.onLeft(renderName)))
  }
}
