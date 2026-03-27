def snippet(x0: Boolean): Int = {
  val x2 = if x0 then {
    val x1 = if x0 then {
      1
    } else {
      2
    }
    x1
  } else {
    0
  }
  x2
}

