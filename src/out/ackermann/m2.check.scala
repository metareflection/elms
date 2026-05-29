def snippet(x0: Int): Int = {
  val x44 = x1(x0)
  x44
}

def x1(x2: Int): Int = {
  val x34 = x2 == 0
  val x33 = if x34 then {
    val x26 = x3(1)
    x26
  } else {
    val x39 = x2 - 1
    val x30 = x1(x39)
    val x31 = x3(x30)
    x31
  }
  x33
}

def x5(x6: Int): Int = {
  val x7 = x6 + 1
  x7
}

def x3(x4: Int): Int = {
  val x16 = x4 == 0
  val x15 = if x16 then {
    val x8 = x5(1)
    x8
  } else {
    val x21 = x4 - 1
    val x12 = x3(x21)
    val x13 = x5(x12)
    x13
  }
  x15
}

