def snippet(x0: Array[Int]): Array[Int] = {
  val x5 = (100 - (x0.length)) > 10
  val x4 = if x5 then {
    val x1 = println("hello")
    ()
  } else {
    ()
  }
  x0
}

