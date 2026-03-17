package lms.core

import lms.ir.Op

abstract class Driver extends Base {
  protected val d: Dialect
  protected type Exp = d.Exp
  type Rep[T] = Exp

  def unit[A:DslType](x: A): Rep[A] = d.lift(x)
  def fun[A, B](f: Rep[A] => Rep[B]): Rep[A => B] = {
    val name = d.fresh()
    d.root(Vector(name)) { f(unsafeWrap(d.v(name))) }
  }

  def unsafeWrap[T](exp: Exp): Rep[T] = exp
  def unsafeUnwrap[T](rep: Rep[T]): Exp = rep
  def unsafeRegister(op: Op, children: Exp*): Exp = d.reflect(op, children.toVector)
}
