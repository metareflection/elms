package lms.core

import lms.core.Op._

trait BooleanOps extends Base {
  extension (lhs: Rep[Boolean])
    def &&(rhs: Rep[Boolean]): Rep[Boolean] =
      unsafeReflect(And, lhs, rhs)
    def ||(rhs: Rep[Boolean]): Rep[Boolean] =
      unsafeReflect(Or, lhs, rhs)
}
