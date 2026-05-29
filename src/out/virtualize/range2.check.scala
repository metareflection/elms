def snippet(x0: Int): Unit = {
  val x5 = (0 until x0).start
  val x6 = (0 until x0).end
  val x4 = for (x1 <- x5 until x6) {
    val x2 = println(x1)
    x2
  }
  
  ()
}

