package lms.helpers

import lms.core.{Driver, Typable}
import lms.pipeline, pipeline.simple
import lms.pipeline.eqsat, eqsat.Rule
import lms.codegen.{Backend, ScalaCodegen}

abstract class SnippetDriver[A: Typable, B: Typable] extends Driver {
  val codegen: Backend

  def snippet(x: Rep[A]): Rep[B]

  def code: String = {
    fun[A, B]("snippet") { x => snippet(x) }
    codegen.render(extract())
  }
}

abstract class SimpleDriver[A: Typable, B: Typable] extends SnippetDriver[A, B] {
  override val builder = pipeline.simple.Builder()
  override val codegen = ScalaCodegen()
}

abstract class OptimizingDriver[A: Typable, B: Typable](rules: Seq[Rule])
    extends SnippetDriver[A, B] {
  override val builder = eqsat.Builder(eqsat.Builder.Config(rules))
  override val codegen = ScalaCodegen()
}
