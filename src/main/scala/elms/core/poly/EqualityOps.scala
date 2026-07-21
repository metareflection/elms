package elms.core.poly

import annotation.implicitNotFound

trait EqualityOps extends BooleanOps {
  @implicitNotFound("`Rep`s should not be compared using `==`, use `===` instead.")
  sealed trait NoRepEquals[T]

  extension [T](lhs: Rep[T])(using CanEqual[T, T])
    def ===(rhs: Rep[T]): Rep[Boolean]
    def !==(rhs: Rep[T]): Rep[Boolean] = !(lhs === rhs)

  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[Rep[T], Rep[T]] =
    CanEqual.derived
  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[T, Rep[T]] = CanEqual.derived
  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[Rep[T], T] = CanEqual.derived
}
