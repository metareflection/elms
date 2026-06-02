package elms.core

import annotation.implicitNotFound

import elms.runtime.Log
import elms.core.Op._

trait IntegerOps extends Base {
  extension (lhs: Rep[Int])
    def +(rhs: Rep[Int]): Rep[Int] = unsafeReflect(Plus, lhs, rhs)
    def -(rhs: Rep[Int]): Rep[Int] = unsafeReflect(Minus, lhs, rhs)
    def *(rhs: Rep[Int]): Rep[Int] = unsafeReflect(Times, lhs, rhs)
    def <(rhs: Rep[Int]): Rep[Boolean] = unsafeReflect(Lt, lhs, rhs)
    def >(rhs: Rep[Int]): Rep[Boolean] = unsafeReflect(Gt, lhs, rhs)
    def <=(rhs: Rep[Int]): Rep[Boolean] = unsafeReflect(Le, lhs, rhs)
    def >=(rhs: Rep[Int]): Rep[Boolean] = unsafeReflect(Ge, lhs, rhs)
}
