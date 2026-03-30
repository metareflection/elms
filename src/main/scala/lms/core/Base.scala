package lms.core

import scala.Conversion

trait Base {
  type Rep[+T]
  protected type Exp

  def unit[A: Liftable](x: A): Rep[A]

  def fun[A: Typable, B: Typable](name: Option[String])(
      f: Rep[A] => Rep[B]
  ): Rep[A => B]
  def lam[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B]

  def fun[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B] = fun(None)(f)
  def fun[A: Typable, B: Typable](name: String)(f: Rep[A] => Rep[B]): Rep[A => B] =
    fun(Some(name))(f)

  def region[A](exp: => Rep[A]): Rep[A]

  given [A](using w: Liftable[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegister(op: Op, children: Exp*): Exp

  def unsafeLift[A: Liftable](x: A): Exp = unsafeUnwrap(unit(x))

  def unsafeReflect[T](op: Op, children: Rep[Any]*): Rep[T] =
    unsafeWrap(unsafeRegister(op, children.map(unsafeUnwrap)*))

  def unsafeFresh[T](): Rep[T]
  def unsafeDeclare[T](name: String): Rep[T]
}
