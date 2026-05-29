def snippet(x0: Boolean): Boolean = {
  val x1 = println("a")
  val x6 = println("a")
  val x13 = if x0 then {
    true
  } else {
    false
  }
  val x19 = if x13 then {
    true
  } else {
    val x17 = if x0 then {
      true
    } else {
      false
    }
    x17
  }
  x19
}

