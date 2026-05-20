package elms.helpers

import elms.core.{Driver, Typable, tree as ast}
import elms.pipeline, pipeline.simple
import elms.pipeline.eqsat, eqsat.Rule
import elms.pipeline.Propagate
import elms.codegen.{Backend, ScalaCodegen}
import elms.util.Plumbing.*

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

  override def extract(): ast.Program = {
    val prog = super.extract()
    ast.Program(
      prog.functions.map(_.mapRight(_.map(Propagate.run))),
      prog.staticData
    )
  }
}
