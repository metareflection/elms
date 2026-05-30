object _hel_s {
  def snippet(x0: String): Boolean = {
      val x105 = {var x41: Int = -1
      var x42: Boolean = false
      val x102 = while {
        val x43 = x42
        val x147 = !x43
        val x47 = if x147 then {
          val x45 = x41
          val x150 = x45 < (x0.length)
          x150
        } else {
          false
        }
        x47
      } do {
        val x49 = x41
        val x153 = x49 + 1
        val x50 = x41 = x153
        val x51 = x41
        val x154 = x51 < (x0.length)
        val x57 = if x154 then {
          val x55 = {val x158 = 'h' == (x0.charAt(x51))
          x158}
          x55
        } else {
          false
        }
        val x99 = if x57 then {
          val x161 = (x51 + 1) < (x0.length)
          val x63 = if x161 then {
            val x61 = {val x165 = 'e' == (x0.charAt((x51 + 1)))
            x165}
            x61
          } else {
            false
          }
          val x96 = if x63 then {
            val x168 = (x51 + 1) + 1
            var x64: Int = x168
            val x65 = x64
            var x66: Boolean = true
            var x67: Boolean = false
            val x88 = while {
              val x68 = x67
              val x172 = !x68
              val x72 = if x172 then {
                val x70 = x66
                val x175 = !x70
                x175
              } else {
                false
              }
              val x76 = if x72 then {
                val x74 = x64
                val x178 = x74 < (x0.length)
                x178
              } else {
                false
              }
              x76
            } do {
              val x78 = x64
              val x81 = {val x181 = 'l' == (x0.charAt(x78))
              x181}
              val x183 = !x81
              val x82 = x67 = x183
              val x83 = x64
              val x184 = x83 + 1
              val x84 = x64 = x184
              val x85 = x64
              val x86 = x66 = true
              ()
            }
            
            val x89 = x67
            val x188 = !x89
            val x93 = if x188 then {
              val x91 = x66
              x91
            } else {
              false
            }
            x93
          } else {
            false
          }
          x96
        } else {
          false
        }
        val x100 = x42 = x99
        ()
      }
      
      val x103 = x42
      x103}
      x105
    }


}