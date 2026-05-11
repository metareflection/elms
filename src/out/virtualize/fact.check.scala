def snippet(x0: Int): Int = {
  val x11 = x1(x0)
  x11
}

def x1(x2: Int): Int = {
  val x3 = 0
  val x4 = x2 == x3
  val x10 = if x4 then {
    val x5 = 1
    x5
  } else {
    val x6 = 1
    val x7 = x2 - x6
    val x9 = x1(x7)
    x9
  }
  x10
}

