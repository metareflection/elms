def snippet(x0: Int): Int = {
  val x1 = x0 == 1
  val x2 = if x1 then {
    1
  } else {
    x0
  }
  x2
}

