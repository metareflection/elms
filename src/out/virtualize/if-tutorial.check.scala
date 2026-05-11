def snippet(x0: Int): Int = {
  val x1 = 1
  val x2 = x0 == x1
  val x4 = if x2 then {
    val x3 = 2
    x3
  } else {
    x0
  }
  x4
}

