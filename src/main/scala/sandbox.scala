package lms

import lms.prelude._
import lms.core.{PrimitiveOps, Driver}
import lms.helpers.SnippetDriver

@virtualize
trait Dsl extends PrimitiveOps {
  def pow(x: Rep[Int], n: Int): Rep[Int] = {
    if n == 0 then 1 else x * pow(x, n-1)
  }
}

object Playground extends SnippetDriver[Int, Int] with Dsl {
  def snippet(x: Rep[Int]) = pow(x, 3)

  @main def main() = {
    println(code)
  }
}
