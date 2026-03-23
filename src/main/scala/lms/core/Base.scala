package lms.core

import scala.Conversion

trait Base {
  type Rep[T]
  protected type Exp

  def unit[A: Liftable](x: A): Rep[A]
  def fun[A, B](f: Rep[A] => Rep[B]): Rep[A => B]

  def region[A](exp: => Rep[A]): Rep[A]

  given [A](using w: Liftable[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegister(op: Op, children: Exp*): Exp

  def unsafeLift[A: Liftable](x: A): Exp = unsafeUnwrap(unit(x))

  def unsafeReflect[T](op: Op, children: Exp*): Rep[T] =
    unsafeWrap(unsafeRegister(op, children*))
}
