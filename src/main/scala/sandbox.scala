package lms

import lms.core.macros.virtualize
import lms.core.{PrimitiveOps, LiftInt, Driver}
import lms.ir.anf.Anf
import lms.codegen.ScalaCodegen

@virtualize
trait Dsl extends PrimitiveOps {
  def pow(x: Rep[Int], n: Int): Rep[Int] = {
    if n == 0 then 1 else x * pow(x, n-1)
  }
}

object Playground extends Driver with Dsl {
  override val d: lms.ir.Dialect = Anf

  def snippet(x: Rep[Int]): Rep[Int] = pow(x, 5)

  @main def main() = {
    println((new ScalaCodegen()).render(d.extract()))
  }
}
