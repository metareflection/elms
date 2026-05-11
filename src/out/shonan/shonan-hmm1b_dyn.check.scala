def snippet(x0: Array[Int]): Array[Int] = {
  val x1 = 100
  val x2 = x0.length
  val x3 = x1 - x2
  val x4 = 10
  val x5 = x3 > x4
  val x10 = if x5 then {
    val x6 = "hello"
    val x7 = println(x6)
    val x8 = ()
    x8
  } else {
    val x9 = ()
    x9
  }
  x0
}

