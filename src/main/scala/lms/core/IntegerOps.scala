package lms.core

import annotation.implicitNotFound

import lms.runtime.Log
import lms.core.Op._

trait IntegerOps extends Base {
  extension (lhs: Rep[Int])
    def +(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Plus, lhs, rhs)
    def -(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Minus, lhs, rhs)
    def *(rhs: Rep[Int]): Rep[Int] =
      unsafeReflect(Times, lhs, rhs)
}
