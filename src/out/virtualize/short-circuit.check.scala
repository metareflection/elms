def snippet(x0: Unit): Boolean = {
  val x1 = "a"
  val x2 = println(x1)
  val x3 = false
  val x7 = x3 && {val x4 = "b"
  val x5 = println(x4)
  val x6 = true
  x6}
  val x8 = "a"
  val x9 = println(x8)
  val x10 = true
  val x14 = x10 || {val x11 = "b"
  val x12 = println(x11)
  val x13 = false
  x13}
  val x15 = x7 && x14
  x15
}

