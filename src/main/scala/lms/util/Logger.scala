package lms.util

trait Logger {
  def info(msg: String)(using pos: SourceContext): Unit
  def warning(msg: String)(using pos: SourceContext): Unit
  def error(msg: String)(using pos: SourceContext): Unit
  def debug(msg: String)(using pos: SourceContext): Unit
}

object Logger {
  def toStderr(prefix: String, msg: String)(using pos: SourceContext): Unit = System.err
    .println(s"[${pos.render}] $prefix: $msg")

  def toStdout(prefix: String, msg: String)(using pos: SourceContext): Unit = System.out
    .println(s"[${pos.render}] $prefix: $msg")

  def debugStderr(msg: String)(using SourceContext): Unit = toStderr("Debug", msg)
  def infoStderr(msg: String)(using SourceContext): Unit = toStderr("Info", msg)
  def warnStderr(msg: String)(using SourceContext): Unit = toStderr("Warning", msg)
  def errorStderr(msg: String)(using SourceContext): Unit = toStderr("Error", msg)

  val default = new Logger {
    def debug(msg: String)(using pos: SourceContext): Unit = {}
    def info(msg: String)(using pos: SourceContext): Unit = {}
    def warning(msg: String)(using pos: SourceContext): Unit = warnStderr(msg)
    def error(msg: String)(using pos: SourceContext): Unit = errorStderr(msg)
  }

  val debug = new Logger {
    def debug(msg: String)(using pos: SourceContext): Unit = debugStderr(msg)
    def info(msg: String)(using pos: SourceContext): Unit = infoStderr(msg)
    def warning(msg: String)(using pos: SourceContext): Unit = warnStderr(msg)
    def error(msg: String)(using pos: SourceContext): Unit = errorStderr(msg)
  }
}
