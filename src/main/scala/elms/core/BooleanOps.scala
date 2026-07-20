package elms.core

import elms.core.Op._

trait BooleanOps extends Base with poly.BooleanOps {
  extension (lhs: Rep[Boolean])
    def &&(rhs: => Rep[Boolean]): Rep[Boolean] = unsafeReflect(And, region(lhs), region(rhs))
    def ||(rhs: => Rep[Boolean]): Rep[Boolean] = unsafeReflect(Or, region(lhs), region(rhs))
    def unary_! = (unsafeReflect(Not, lhs): Rep[Boolean])
}
