package lms.helpers

import lms.core.{Driver, Typable}
import lms.ir, ir.simple
import lms.codegen.{Backend, ScalaCodegen}

abstract class SnippetDriver[A: Typable, B: Typable](
    irBuilder: ir.Builder = new simple.Builder(),
    codegen: Backend = new ScalaCodegen()
) extends Driver {
  override val builder = irBuilder
  def snippet(x: Rep[A]): Rep[B]

  lazy val code: String = {
    fun[A, B]("snippet") { x => snippet(x) }
    codegen.render(extract())
  }
}
