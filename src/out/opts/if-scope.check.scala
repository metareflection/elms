def snippet(x0: Int): Int = {
  var x1: Int = x0
  var x2: Int = 0
  val x3 = x1
  val x16 = x3 + 2
  val x4 = x1 = x16
  val x5 = x1
  val x17 = x5 > 0
  val x11 = if x17 then {
    val x6 = x2
    val x7 = x1
    val x19 = x6 + x7
    val x8 = x2 = x19
    ()
  } else {
    ()
  }
  val x12 = x2
  x12
}

