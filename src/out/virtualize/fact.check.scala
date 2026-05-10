def snippet(x0: Int): Int = {
  val x8 = x1(x0)
  x8
}

def x1(x2: Int): Int = {
  val x3 = x2 == 0
  val x7 = if x3 then {
    1
  } else {
    val x4 = x2 - 1
    val x6 = x1(x4)
    x6
  }
  x7
}

