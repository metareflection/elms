package lms.codegen

import lms.util.IndentedWriter

/** General formatting configuration for code generation.
  *
  * @param baseIndentLevel
  *   The number of tabstops to indent the entire generated program by.
  * @param indentKind
  */
case class Config(baseIndentLevel: Int, indentKind: IndentedWriter.IndentKind)

object Config {
  val tabs: IndentedWriter.IndentKind = IndentedWriter.UseTabs
  def spaces(i: Int): IndentedWriter.IndentKind = IndentedWriter.UseSpaces(2)

  val scalaDefault = Config(0, spaces(2))
}
