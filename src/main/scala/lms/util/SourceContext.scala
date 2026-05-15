package lms.util

import scala.quoted.*

case class SourceContext(fileName: String, line: Int, column: Int) {
  def render(depth: Int): String =
    s"${fileName.split("/").takeRight(depth).mkString("/")}:$line:$column"
}

object SourceContext {
  inline given generate: SourceContext = ${ generateImpl }

  private def generateImpl(using Quotes): Expr[SourceContext] = {
    import quotes.reflect.*

    val pos = Position.ofMacroExpansion
    val file = pos.sourceFile.path
    val line = pos.startLine + 1
    val column = pos.startColumn + 1

    '{ SourceContext(${ Expr(file) }, ${ Expr(line) }, ${ Expr(column) }) }
  }
}
