package elms.core

import elms.core.Op._

trait EqualityOps extends BooleanOps with poly.EqualityOps {
  extension [T](lhs: Rep[T])(using CanEqual[T, T])
    def ===(rhs: Rep[T]): Rep[Boolean] = unsafeReflect(Equals, lhs, rhs)
}
