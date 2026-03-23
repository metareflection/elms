package lms.core

import lms.core.Op._

trait PrimitiveOps extends Base {
  def __ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] =
    unsafeReflect(IfThenElse, unsafeUnwrap(c), unsafeUnwrap(t), unsafeUnwrap(e))

  given __virtualizedBoolConvInternal: Conversion[Rep[Boolean], Boolean] with
    def apply(x: Rep[Boolean]) = {
      throw new RuntimeException(
        "attempted to call __virtualizedBoolConvInternal (did you forget to virtualize?)"
      );
    }

  extension (lhs: Rep[Int])
    def +(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Plus, unsafeUnwrap(lhs), unsafeUnwrap(rhs))
    def -(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Minus, unsafeUnwrap(lhs), unsafeUnwrap(rhs))
    def *(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Times, unsafeUnwrap(lhs), unsafeUnwrap(rhs))

  given [T](using CanEqual[T, T]): CanEqual[Rep[T], Rep[T]] = CanEqual.derived
  given [T](using CanEqual[T, T]): CanEqual[T, Rep[T]] = CanEqual.derived
  given [T](using CanEqual[T, T]): CanEqual[Rep[T], T] = CanEqual.derived
}
