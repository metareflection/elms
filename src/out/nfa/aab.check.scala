def snippet(x0: Unit): Automaton[Char, Boolean] = {
  val x1 = false
  lazy val x2: (Char => Automaton[Char, Boolean]) = (x3: Char) => {
    val x4 = 'A'
    val x5 = x3 == x4
    val x38 = if x5 then {
      val x6 = false
      lazy val x7: (Char => Automaton[Char, Boolean]) = (x8: Char) => {
        val x9 = 'A'
        val x10 = x8 == x9
        val x33 = if x10 then {
          val x11 = false
          lazy val x12: (Char => Automaton[Char, Boolean]) = (x13: Char) => {
            val x14 = 'A'
            val x15 = x13 == x14
            val x28 = if x15 then {
              val x16 = false
              val x18 = Automaton(x16, x12)
              x18
            } else {
              val x19 = 'B'
              val x20 = x13 == x19
              val x27 = if x20 then {
                val x21 = true
                val x23 = Automaton(x21, x2)
                x23
              } else {
                val x24 = false
                val x26 = Automaton(x24, x2)
                x26
              }
              x27
            }
            x28}
          
          val x29 = Automaton(x11, x12)
          x29
        } else {
          val x30 = false
          val x32 = Automaton(x30, x2)
          x32
        }
        x33}
      
      val x34 = Automaton(x6, x7)
      x34
    } else {
      val x35 = false
      val x37 = Automaton(x35, x2)
      x37
    }
    x38}
  
  val x39 = Automaton(x1, x2)
  x39
}

