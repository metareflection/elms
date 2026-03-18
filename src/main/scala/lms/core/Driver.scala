package lms.core

import lms.ir.{Op, Dialect}

abstract class Driver extends Base {
  protected val d: Dialect
  protected type Exp = d.Exp
  type Rep[T] = Exp

  def unit[A:Liftable](x: A): Rep[A] = d.lift(x)
  def fun[A:Typable, B:Typable](f: Rep[A] => Rep[B]): Rep[A => B] = {
    val name = d.fresh()
    val argty = summon[Typable[A]].identity
    val outty = summon[Typable[B]].identity
    d.lam(false, Vector((name, argty)), outty) { f(unsafeWrap(d.variable(name))) }
  }

  def unsafeWrap[T](exp: Exp): Rep[T] = exp
  def unsafeUnwrap[T](rep: Rep[T]): Exp = rep
  def unsafeRegister(op: Op, children: Exp*): Exp = d.reflect(op, children.toVector)
}
