package lms.helpers

import lms.core.{Driver, Typable}
import lms.ir.{anf, Builder}
import lms.codegen.{Backend, ScalaCodegen}

abstract class SnippetDriver[A: Typable, B: Typable](
    dialect: Builder = new anf.Builder(),
    codegen: Backend = new ScalaCodegen()
) extends Driver {
  override val d = dialect
  def snippet(x: Rep[A]): Rep[B]

  lazy val code: String = {
    fun[A, B]("snippet")(snippet)
    codegen.render(extract())
  }
}
