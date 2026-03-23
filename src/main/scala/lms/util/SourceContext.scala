package lms.util

import scala.quoted.*

case class SourceContext(fileName: String, line: Int, column: Int) {
  def render: String = s"$fileName:$line:$column"
}

object SourceContext {
  inline given generate: SourceContext = ${ generateImpl }

  private def generateImpl(using Quotes): Expr[SourceContext] = {
    import quotes.reflect.*

    val pos = Position.ofMacroExpansion
    val file = pos.sourceFile.jpath.getFileName.toString
    val line = pos.startLine + 1
    val column = pos.startColumn + 1

    // TODO
    '{ SourceContext(${Expr(file)}, ${Expr(line)}, ${Expr(column)}) }
  }
}
