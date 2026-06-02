package elms.codegen

import elms.core.Type
import elms.core.tree as ast
import elms.util.IndentedWriter

abstract class Backend(cfg: Config) {
  def emit(prog: ast.Program, out: java.io.PrintStream): Unit

  def render(prog: ast.Program): String = {
    val w = new java.io.ByteArrayOutputStream()
    emit(prog, new java.io.PrintStream(w))
    w.toString("utf-8")
  }

  protected def renderType(ty: Type): String

  protected def makeIndentedWriter(out: java.io.PrintStream): IndentedWriter =
    IndentedWriter(out, cfg.baseIndentLevel, cfg.indentKind)
}
