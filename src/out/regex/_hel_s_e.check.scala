object _hel_s_e {
  def snippet(x0: String): Boolean = {
      val x105 = {var x41: Int = -1
      var x42: Boolean = false
      val x102 = while {
        val x43 = x42
        val x149 = !x43
        val x47 = if x149 then {
          val x45 = x41
          val x152 = x45 < (x0.length)
          x152
        } else {
          false
        }
        x47
      } do {
        val x49 = x41
        val x155 = x49 + 1
        val x50 = x41 = x155
        val x51 = x41
        val x156 = x51 < (x0.length)
        val x57 = if x156 then {
          val x55 = {val x160 = 'h' == (x0.charAt(x51))
          x160}
          x55
        } else {
          false
        }
        val x99 = if x57 then {
          val x163 = (x51 + 1) < (x0.length)
          val x63 = if x163 then {
            val x61 = {val x167 = 'e' == (x0.charAt((x51 + 1)))
            x167}
            x61
          } else {
            false
          }
          val x96 = if x63 then {
            val x170 = (x51 + 1) + 1
            var x64: Int = x170
            val x65 = x64
            val x172 = x65 == (x0.length)
            var x66: Boolean = x172
            var x67: Boolean = false
            val x88 = while {
              val x68 = x67
              val x174 = !x68
              val x72 = if x174 then {
                val x70 = x66
                val x177 = !x70
                x177
              } else {
                false
              }
              val x76 = if x72 then {
                val x74 = x64
                val x180 = x74 < (x0.length)
                x180
              } else {
                false
              }
              x76
            } do {
              val x78 = x64
              val x81 = {val x184 = 'l' == (x0.charAt(x78))
              x184}
              val x186 = !x81
              val x82 = x67 = x186
              val x83 = x64
              val x187 = x83 + 1
              val x84 = x64 = x187
              val x85 = x64
              val x189 = x85 == (x0.length)
              val x86 = x66 = x189
              ()
            }
            
            val x89 = x67
            val x192 = !x89
            val x93 = if x192 then {
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