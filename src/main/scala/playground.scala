package lms

import lms.prelude._
import lms.prelude.given
import lms.helpers._

trait Dsl extends DslOps {
  @virtualize
  def pow(x: Rep[Int], n: Int): Rep[Int] = { if n == 0 then 1 else x * pow(x, n - 1) }

  def foo(f: Rep[Int => Int]) = f(1)
}

object Playground extends SnippetDriver[Int, Int] with Dsl {
  def snippet(x: Rep[Int]) = pow(x, 4)
}

@main
def main() = {
  println(Playground.code)
}
