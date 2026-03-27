package lms.util

trait Logger {
  def info(msg: String)(using pos: SourceContext): Unit
  def warning(msg: String)(using pos: SourceContext): Unit
  def error(msg: String)(using pos: SourceContext): Unit
}

object Logger {
  val default = new Logger {
    def info(msg: String)(using pos: SourceContext): Unit = System.err
      .println(s"[$pos.render]: $msg")

    def warning(msg: String)(using pos: SourceContext): Unit = System.err
      .println(s"[$pos.render]: $msg")

    def error(msg: String)(using pos: SourceContext): Unit = System.err
      .println(s"[$pos.render]: $msg")
  }
}
