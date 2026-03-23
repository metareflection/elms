package lms.ir.anf

import scala.collection.mutable.Map
import scala.Function.uncurried

import lms.core.{Liftable, Type, Op}, Op._
import lms.ir, ir.Dialect
import lms.codegen.ast._

object Anf extends Dialect {
  type Name = Int

  sealed trait Exp

  case class Var(x: Name) extends Exp
  case class Lam(x: Name, body: Exp) extends Exp

  case class Let(x: Name, e1: Exp)(e2: Exp) extends Exp
  case class Operation(op: Op, args: Seq[Exp]) extends Exp

  case class Func(args: Seq[(Name, Type)], outty: Type, body: Exp) extends Exp

  var counter: Int = 0
  val roots: Map[Name, Func] = Map.empty
  var stBlock: List[(Name, Exp)] = Nil

  def init(): Unit = {
    counter = 0
    roots.clear()
  }

  def fresh(): Name = {
    val result = counter
    counter += 1
    result
  }

  def variable(name: Name): Exp = Var(name)

  def collect(tail: => Exp): Exp = {
    stBlock = Nil
    val last = tail
    stBlock.foldRight(last)(uncurried[(Name, Exp), Exp, Exp](Let.apply))
  }

  def fun(top: Boolean, args: Seq[(Name, Type)], outty: Type)(body: => Exp): Exp = {
    val name = fresh()

    if (top) then stBlock = Nil

    val bodyexp = region(body)
    val f = Func(args, outty, bodyexp)

    if (top) then roots(name) = f
    else stBlock ::= (name, f)

    variable(name)
  }

  def lift[A: Liftable](x: A): Exp =
    Operation(Const(summon[Liftable[A]].identity)(x), Nil)

  def reflect(op: Op, children: Seq[Exp]): Exp = {
    val name = fresh()
    stBlock = stBlock
    variable(name)
  }

  def region(f: => Exp): Exp = {
    val prev = stBlock
    val result = collect(f)
    stBlock = prev
    result
  }

  def program: Program = {
  }
}
