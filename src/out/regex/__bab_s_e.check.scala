object __bab_s_e {
  def snippet(x0: String): Boolean = {
      val x106 = ("^ab*$".charAt(0)) == '^'
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
        x39
      } else {
        var x41: Int = -1
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
            val x55 = {val x160 = '^' == (x0.charAt(x51))
            x160}
            x55
          } else {
            false
          }
          val x99 = if x57 then {
            val x163 = (x51 + 1) < (x0.length)
            val x63 = if x163 then {
              val x61 = {val x167 = 'a' == (x0.charAt((x51 + 1)))
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
                val x81 = {val x184 = 'b' == (x0.charAt(x78))
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
        x103
      }
      x105
    }


}