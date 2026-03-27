package lms.test

import lms.prelude._
import lms.helpers.DslOps

// Tests to ensure that implicit resolution is set up correctly.
trait RepFunTypecheckSuite extends DslOps {
  def void(f: Rep[() => Int]) = f()
  def unary(f: Rep[Int => Int]) = f(1)
  def binary(f: Rep[(Int, String) => Int]) = f(1, "")
  def trinary(f: Rep[(Int, String, Boolean) => Int]) = f(1, "", false)
}
