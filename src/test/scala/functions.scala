package elms.test

import scala.language.implicitConversions

import elms.prelude._
import elms.prelude.given
import elms.helpers.DslOps

// Tests to ensure that implicit resolution is set up correctly.
// These should all typecheck and compile.
trait RepFunTypecheckSuite extends DslOps {
  def void(f: Rep[() => Int]) = f()
  def unary(f: Rep[Int => Int]) = f(1)
  def binary(f: Rep[(Int, String) => Int]) = f(1, "")
  def trinary(f: Rep[(Int, String, Boolean) => Int]) = f(1, "", false)
}
