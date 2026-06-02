package elms.util

case class Counter() {
  var x = 0

  def tick(): Int = {
    val result = x
    x += 1
    return result
  }
}
