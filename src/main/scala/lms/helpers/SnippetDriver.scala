package lms.helpers

import lms.core.{Driver, Typable}
import lms.ir, ir.simple
import lms.codegen.{Backend, ScalaCodegen}

abstract class SnippetDriver[A: Typable, B: Typable] extends Driver {
  val codegen: Backend

  def snippet(x: Rep[A]): Rep[B]

  lazy val code: String = {
    fun[A, B]("snippet") { x => snippet(x) }
    codegen.render(extract())
  }
}

abstract class SimpleDriver[A: Typable, B: Typable] extends SnippetDriver[A, B] {
  override val builder = ir.simple.Builder()
  override val codegen = ScalaCodegen()
}
