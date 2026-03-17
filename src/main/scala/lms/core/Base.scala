package lms.core

import scala.Conversion

import lms.ir.Op

trait Base {
  type Rep[T]
  protected type Exp

  def unit[A: DslType](x: A): Rep[A]
  def fun[A, B](f: Rep[A] => Rep[B]): Rep[A => B]

  given [A](using w: DslType[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegister(op: Op, children: Exp*): Exp

  def unsafeLift[A: DslType](x: A): Exp = unsafeUnwrap(unit(x))

  def unsafeReflect[T](op: Op, children: Exp*): Rep[T] =
    unsafeWrap(unsafeRegister(op, children*))
}
