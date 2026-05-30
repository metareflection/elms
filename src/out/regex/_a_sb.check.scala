object _a_sb {
  def snippet(x0: String): Boolean = {
      val x85 = ("a*b".charAt(0)) == '^'
      val x84 = if x85 then {
        val x86 = 0 < (x0.length)
        val x6 = if x86 then {
          val x4 = {val x91 = '*' == (x0.charAt(0))
          x91}
          x4
        } else {
          false
        }
        val x18 = if x6 then {
          val x95 = 1 < (x0.length)
          val x12 = if x95 then {
            val x10 = {val x100 = 'b' == (x0.charAt(1))
            x100}
            x10
          } else {
            false
          }
          val x15 = if x12 then {
            true
          } else {
            false
          }
          x15
        } else {
          false
        }
        x18
      } else {
        var x20: Int = -1
        var x21: Boolean = false
        val x81 = while {
          val x22 = x21
          val x112 = !x22
          val x26 = if x112 then {
            val x24 = x20
            val x115 = x24 < (x0.length)
            x115
          } else {
            false
          }
          x26
        } do {
          val x28 = x20
          val x118 = x28 + 1
          val x29 = x20 = x118
          val x30 = x20
          var x31: Int = x30
          val x32 = x31
          val x121 = x32 < (x0.length)
          val x38 = if x121 then {
            val x36 = {val x125 = 'b' == (x0.charAt(x32))
            x125}
            x36
          } else {
            false
          }
          val x41 = if x38 then {
            true
          } else {
            false
          }
          var x42: Boolean = x41
          var x43: Boolean = false
          val x73 = while {
            val x44 = x43
            val x131 = !x44
            val x48 = if x131 then {
              val x46 = x42
              val x134 = !x46
              x134
            } else {
              false
            }
            val x52 = if x48 then {
              val x50 = x31
              val x137 = x50 < (x0.length)
              x137
            } else {
              false
            }
            x52
          } do {
            val x54 = x31
            val x57 = {val x141 = 'a' == (x0.charAt(x54))
            x141}
            val x143 = !x57
            val x58 = x43 = x143
            val x59 = x31
            val x144 = x59 + 1
            val x60 = x31 = x144
            val x61 = x31
            val x145 = x61 < (x0.length)
            val x67 = if x145 then {
              val x65 = {val x149 = 'b' == (x0.charAt(x61))
              x149}
              x65
            } else {
              false
            }
            val x70 = if x67 then {
              true
            } else {
              false
            }
            val x71 = x42 = x70
            ()
          }
          
          val x74 = x43
          val x157 = !x74
          val x78 = if x157 then {
            val x76 = x42
            x76
          } else {
            false
          }
          val x79 = x21 = x78
          ()
        }
        
        val x82 = x21
        x82
      }
      x84
    }


}