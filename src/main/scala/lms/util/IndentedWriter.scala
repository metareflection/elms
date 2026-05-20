package elms.util

class IndentedWriter(
    out: java.io.PrintStream,
    initial: Int,
    indentKind: IndentedWriter.IndentKind
) {
  var level = initial
  var startOfLine = false

  def emit(s: String): Unit = {
    if startOfLine then out.print(indentKind.render(level))
    out.print(s)
    startOfLine = s.endsWith("\n")
  }

  def emitln(s: String): Unit = emit(s + "\n")

  def indented(f: => Unit): Unit = {
    level += 1
    f
    level -= 1
  }
}

object IndentedWriter {
  sealed trait IndentKind {
    def render(count: Int): String
  }

  case object UseTabs extends IndentKind {
    def render(count: Int): String = "\t" * count
  }

  case class UseSpaces(tabstop: Int) extends IndentKind {
    def render(count: Int): String = " " * (tabstop * count)
  }
}
