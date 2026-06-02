package elms.util

trait Logger {
  def pathDepth = 1
  def info(msg: String)(using pos: SourceContext): Unit
  def warning(msg: String)(using pos: SourceContext): Unit
  def error(msg: String)(using pos: SourceContext): Unit
  def debug(msg: String)(using pos: SourceContext): Unit
}

object Logger {
  def toStderr(prefix: String, msg: String, pathDepth: Int)(using
      pos: SourceContext
  ): Unit = System.err.println(s"[${pos.render(pathDepth)}] $prefix: $msg")

  def toStdout(prefix: String, msg: String, pathDepth: Int)(using
      pos: SourceContext
  ): Unit = System.out.println(s"[${pos.render(pathDepth)}] $prefix: $msg")

  def debugStderr(msg: String, pathDepth: Int)(using SourceContext): Unit =
    toStderr("Debug", msg, pathDepth)
  def infoStderr(msg: String, pathDepth: Int)(using SourceContext): Unit =
    toStderr("Info", msg, pathDepth)
  def warnStderr(msg: String, pathDepth: Int)(using SourceContext): Unit =
    toStderr("Warning", msg, pathDepth)
  def errorStderr(msg: String, pathDepth: Int)(using SourceContext): Unit =
    toStderr("Error", msg, pathDepth)

  val default = new Logger {
    def debug(msg: String)(using pos: SourceContext): Unit = {}
    def info(msg: String)(using pos: SourceContext): Unit = {}
    def warning(msg: String)(using pos: SourceContext): Unit =
      warnStderr(msg, pathDepth)
    def error(msg: String)(using pos: SourceContext): Unit = errorStderr(msg, pathDepth)
  }

  val debug = new Logger {
    def debug(msg: String)(using pos: SourceContext): Unit = debugStderr(msg, pathDepth)
    def info(msg: String)(using pos: SourceContext): Unit = infoStderr(msg, pathDepth)
    def warning(msg: String)(using pos: SourceContext): Unit =
      warnStderr(msg, pathDepth)
    def error(msg: String)(using pos: SourceContext): Unit = errorStderr(msg, pathDepth)
  }
}
