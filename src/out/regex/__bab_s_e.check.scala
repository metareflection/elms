object __bab_s_e {
  def snippet(x0: String): Boolean = {
      val x105 = {val x107 = 0 < (x0.length)
      val x6 = if x107 then {
        val x4 = {val x112 = 'a' == (x0.charAt(0))
        x112}
        x4
      } else {
        false
      }
      val x39 = if x6 then {
        var x7: Int = 1
        val x8 = x7
        val x118 = x8 == (x0.length)
        var x9: Boolean = x118
        var x10: Boolean = false
        val x31 = while {
          val x11 = x10
          val x121 = !x11
          val x15 = if x121 then {
            val x13 = x9
            val x124 = !x13
            x124
          } else {
            false
          }
          val x19 = if x15 then {
            val x17 = x7
            val x127 = x17 < (x0.length)
            x127
          } else {
            false
          }
          x19
        } do {
          val x21 = x7
          val x24 = {val x131 = 'b' == (x0.charAt(x21))
          x131}
          val x133 = !x24
          val x25 = x10 = x133
          val x26 = x7
          val x134 = x26 + 1
          val x27 = x7 = x134
          val x28 = x7
          val x136 = x28 == (x0.length)
          val x29 = x9 = x136
          ()
        }
        
        val x32 = x10
        val x139 = !x32
        val x36 = if x139 then {
          val x34 = x9
          x34
        } else {
          false
        }
        x36
      } else {
        false
      }
      x39}
      x105
    }


}