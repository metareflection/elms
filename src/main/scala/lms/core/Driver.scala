package lms.core

import lms.core.Op
import lms.ir.Dialect

abstract class Driver extends Base {
  protected val d: Dialect
  protected type Exp = d.Exp
  type Rep[T] = Exp

  override def unit[A:Liftable](x: A): Rep[A] = d.lift(x)
  override def fun[A:Typable, B:Typable](f: Rep[A] => Rep[B]): Rep[A => B] = {
    val name = d.fresh()
    val argty = summon[Typable[A]].identity
    val outty = summon[Typable[B]].identity
    d.fun(false, Vector((name, argty)), outty) { f(unsafeWrap(d.variable(name))) }
  }
  override def region[A](exp: => Rep[A]): Rep[A] = unsafeWrap(d.region(exp))

  def topFun[A:Typable, B:Typable](f: Rep[A] => Rep[B]): Rep[A => B] = {
    val name = d.fresh()
    val argty = summon[Typable[A]].identity
    val outty = summon[Typable[B]].identity
    d.fun(true, Vector((name, argty)), outty) { f(unsafeWrap(d.variable(name))) }
  }

  override def unsafeWrap[T](exp: Exp): Rep[T] = exp
  override def unsafeUnwrap[T](rep: Rep[T]): Exp = rep
  override def unsafeRegister(op: Op, children: Exp*): Exp = d.reflect(op, children.toVector)
}
