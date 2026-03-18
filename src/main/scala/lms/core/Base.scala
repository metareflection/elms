package lms.core

import scala.Conversion

import lms.ir

trait Base {
  type Rep[T]
  protected type Exp

  def unit[A: Liftable](x: A): Rep[A]
  def fun[A, B](f: Rep[A] => Rep[B]): Rep[A => B]

  given [A](using w: Liftable[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegister(op: ir.Op, children: Exp*): Exp

  def unsafeLift[A: Liftable](x: A): Exp = unsafeUnwrap(unit(x))

  def unsafeReflect[T](op: ir.Op, children: Exp*): Rep[T] =
    unsafeWrap(unsafeRegister(op, children*))
}
