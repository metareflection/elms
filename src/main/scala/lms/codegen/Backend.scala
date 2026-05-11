package lms.codegen

import lms.pipeline.tree as ast
import lms.util.IndentedWriter

abstract class Backend(cfg: Config) {
  def emit(prog: ast.Program, out: java.io.PrintStream): Unit

  def render(prog: ast.Program): String = {
    val w = new java.io.ByteArrayOutputStream()
    emit(prog, new java.io.PrintStream(w))
    w.toString("utf-8")
  }

  protected def makeIndentedWriter(out: java.io.PrintStream): IndentedWriter =
    IndentedWriter(out, cfg.baseIndentLevel, cfg.indentKind)
}
