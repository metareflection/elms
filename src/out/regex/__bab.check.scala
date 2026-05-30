object __bab {
  def snippet(x0: String): Boolean = {
      val x64 = ("^ab".charAt(0)) == '^'
      val x63 = if x64 then {
        val x65 = 0 < (x0.length)
        val x6 = if x65 then {
          val x4 = {val x70 = 'a' == (x0.charAt(0))
          x70}
          x4
        } else {
          false
        }
        val x18 = if x6 then {
          val x74 = 1 < (x0.length)
          val x12 = if x74 then {
            val x10 = {val x79 = 'b' == (x0.charAt(1))
            x79}
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
        val x60 = while {
          val x22 = x21
          val x91 = !x22
          val x26 = if x91 then {
            val x24 = x20
            val x94 = x24 < (x0.length)
            x94
          } else {
            false
          }
          x26
        } do {
          val x28 = x20
          val x97 = x28 + 1
          val x29 = x20 = x97
          val x30 = x20
          val x98 = x30 < (x0.length)
          val x36 = if x98 then {
            val x34 = {val x102 = '^' == (x0.charAt(x30))
            x102}
            x34
          } else {
            false
          }
          val x57 = if x36 then {
            val x105 = (x30 + 1) < (x0.length)
            val x42 = if x105 then {
              val x40 = {val x109 = 'a' == (x0.charAt((x30 + 1)))
              x109}
              x40
            } else {
              false
            }
            val x54 = if x42 then {
              val x112 = ((x30 + 1) + 1) < (x0.length)
              val x48 = if x112 then {
                val x46 = {val x116 = 'b' == (x0.charAt(((x30 + 1) + 1)))
                x116}
                x46
              } else {
                false
              }
              val x51 = if x48 then {
                true
              } else {
                false
              }
              x51
            } else {
              false
            }
            x54
          } else {
            false
          }
          val x58 = x21 = x57
          ()
        }
        
        val x61 = x21
        x61
      }
      x63
    }


}