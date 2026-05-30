object __bab_s {
  def snippet(x0: String): Boolean = {
      val x106 = ("^ab*".charAt(0)) == '^'
      val x105 = if x106 then {
        val x107 = 0 < (x0.length)
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
          var x9: Boolean = true
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
            val x24 = {val x130 = 'b' == (x0.charAt(x21))
            x130}
            val x132 = !x24
            val x25 = x10 = x132
            val x26 = x7
            val x133 = x26 + 1
            val x27 = x7 = x133
            val x28 = x7
            val x29 = x9 = true
            ()
          }
          
          val x32 = x10
          val x137 = !x32
          val x36 = if x137 then {
            val x34 = x9
            x34
          } else {
            false
          }
          x36
        } else {
          false
        }
        x39
      } else {
        var x41: Int = -1
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
            val x55 = {val x158 = '^' == (x0.charAt(x51))
            x158}
            x55
          } else {
            false
          }
          val x99 = if x57 then {
            val x161 = (x51 + 1) < (x0.length)
            val x63 = if x161 then {
              val x61 = {val x165 = 'a' == (x0.charAt((x51 + 1)))
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
                val x81 = {val x181 = 'b' == (x0.charAt(x78))
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
        x103
      }
      x105
    }


}