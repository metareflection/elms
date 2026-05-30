object _hell {
  def snippet(x0: String): Boolean = {
      val x81 = {var x29: Int = -1
      var x30: Boolean = false
      val x78 = while {
        val x31 = x30
        val x120 = !x31
        val x35 = if x120 then {
          val x33 = x29
          val x123 = x33 < (x0.length)
          x123
        } else {
          false
        }
        x35
      } do {
        val x37 = x29
        val x126 = x37 + 1
        val x38 = x29 = x126
        val x39 = x29
        val x127 = x39 < (x0.length)
        val x45 = if x127 then {
          val x43 = {val x131 = 'h' == (x0.charAt(x39))
          x131}
          x43
        } else {
          false
        }
        val x75 = if x45 then {
          val x134 = (x39 + 1) < (x0.length)
          val x51 = if x134 then {
            val x49 = {val x138 = 'e' == (x0.charAt((x39 + 1)))
            x138}
            x49
          } else {
            false
          }
          val x72 = if x51 then {
            val x141 = ((x39 + 1) + 1) < (x0.length)
            val x57 = if x141 then {
              val x55 = {val x145 = 'l' == (x0.charAt(((x39 + 1) + 1)))
              x145}
              x55
            } else {
              false
            }
            val x69 = if x57 then {
              val x148 = (((x39 + 1) + 1) + 1) < (x0.length)
              val x63 = if x148 then {
                val x61 = {val x152 = 'l' == (x0.charAt((((x39 + 1) + 1) + 1)))
                x152}
                x61
              } else {
                false
              }
              val x66 = if x63 then {
                true
              } else {
                false
              }
              x66
            } else {
              false
            }
            x69
          } else {
            false
          }
          x72
        } else {
          false
        }
        val x76 = x30 = x75
        ()
      }
      
      val x79 = x30
      x79}
      x81
    }


}