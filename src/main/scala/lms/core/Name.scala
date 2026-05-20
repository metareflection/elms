package elms.core

sealed abstract class Name {
  def render(prefix: String): String
}

private case class Named(s: String) extends Name {
  override def render(prefix: String): String = s
}
private case class Fresh(id: Int) extends Name {
  override def render(prefix: String): String = s"$prefix$id"
}

object Name {
  def from(s: String): Name = Named(s)
  def from(i: Int): Name = Fresh(i)
}
