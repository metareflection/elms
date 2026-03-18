package lms.ir.anf

import scala.collection.mutable.Map
import scala.Function.uncurried

import lms.core.Liftable
import lms.ir, ir.Dialect

object Anf extends Dialect {
  type Name = Int

  sealed trait Exp

  case class Var(x: Name) extends Exp
  case class Lam(x: Name, body: Exp) extends Exp
  case class Const(typ: ir.Type)(v: typ.T) extends Exp

  case class Let(x: Name, e1: Exp)(e2: Exp) extends Exp
  case class Op(op: ir.Op, args: Seq[Exp]) extends Exp

  var counter: Int = 0
  val roots: Map[Name, (Exp, Seq[(Name, ir.Type)], ir.Type)] = Map.empty
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
    stBlock.foldRight(tail)(uncurried[(Name, Exp), Exp, Exp](Let.apply))
  }

  def root(args: Seq[(Name, ir.Type)], outty: ir.Type)(body: => Exp): Exp = {
    val name = fresh()
    val bodyexp = collect(body)
    roots(name) = (bodyexp, args, outty)
    variable(name)
  }

  def lift[A: Liftable](x: A): Exp = Const(summon[Liftable[A]].identity)(x)

  def reflect(op: ir.Op, children: Seq[Exp]): Exp = {
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
}
