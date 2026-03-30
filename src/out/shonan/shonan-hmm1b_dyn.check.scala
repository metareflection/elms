def snippet(x0: Array[Int]): Array[Int] = {
  val x1 = x0.length
  val x2 = 100 - x1
  val x3 = x2 > 10
  val x5 = if x3 then {
    val x4 = println("hello")
    ()
  } else {
    ()
  }
  x0
}

