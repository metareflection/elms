def snippet(x0: Int): Unit = {
  lazy val x1: (Int => Unit) = (x2: Int) => {
    val x3 = 3
    val x4 = x2 + x3
    val x5 = println(x4)
    x5}
  
  val x6 = x1(x0)
  x6
}

