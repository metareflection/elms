package lms.helpers

import lms.core.{Driver, Typable}
import lms.pipeline, pipeline.simple
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
  override val builder = pipeline.simple.Builder()
  override val codegen = ScalaCodegen()
}
