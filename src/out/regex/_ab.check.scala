object _ab {
  def snippet(x0: String): Boolean = {
      val x45 = {var x11: Int = -1
      var x12: Boolean = false
      val x42 = while {
        val x13 = x12
        val x62 = !x13
        val x17 = if x62 then {
          val x15 = x11
          val x65 = x15 < (x0.length)
          x65
        } else {
          false
        }
        x17
      } do {
        val x19 = x11
        val x68 = x19 + 1
        val x20 = x11 = x68
        val x21 = x11
        val x69 = x21 < (x0.length)
        val x27 = if x69 then {
          val x25 = {val x73 = 'a' == (x0.charAt(x21))
          x73}
          x25
        } else {
          false
        }
        val x39 = if x27 then {
          val x76 = (x21 + 1) < (x0.length)
          val x33 = if x76 then {
            val x31 = {val x80 = 'b' == (x0.charAt((x21 + 1)))
            x80}
            x31
          } else {
            false
          }
          val x36 = if x33 then {
            true
          } else {
            false
          }
          x36
        } else {
          false
        }
        val x40 = x12 = x39
        ()
      }
      
      val x43 = x12
      x43}
      x45
    }


}