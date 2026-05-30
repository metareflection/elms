object __bhello_e {
  def snippet(x0: String): Boolean = {
      val x118 = ("^hello$".charAt(0)) == '^'
      val x117 = if x118 then {
        val x119 = 0 < (x0.length)
        val x6 = if x119 then {
          val x4 = {val x124 = 'h' == (x0.charAt(0))
          x124}
          x4
        } else {
          false
        }
        val x45 = if x6 then {
          val x128 = 1 < (x0.length)
          val x12 = if x128 then {
            val x10 = {val x133 = 'e' == (x0.charAt(1))
            x133}
            x10
          } else {
            false
          }
          val x42 = if x12 then {
            val x137 = 2 < (x0.length)
            val x18 = if x137 then {
              val x16 = {val x142 = 'l' == (x0.charAt(2))
              x142}
              x16
            } else {
              false
            }
            val x39 = if x18 then {
              val x146 = 3 < (x0.length)
              val x24 = if x146 then {
                val x22 = {val x151 = 'l' == (x0.charAt(3))
                x151}
                x22
              } else {
                false
              }
              val x36 = if x24 then {
                val x155 = 4 < (x0.length)
                val x30 = if x155 then {
                  val x28 = {val x160 = 'o' == (x0.charAt(4))
                  x160}
                  x28
                } else {
                  false
                }
                val x33 = if x30 then {
                  val x164 = 5 == (x0.length)
                  x164
                } else {
                  false
                }
                x33
              } else {
                false
              }
              x36
            } else {
              false
            }
            x39
          } else {
            false
          }
          x42
        } else {
          false
        }
        x45
      } else {
        var x47: Int = -1
        var x48: Boolean = false
        val x114 = while {
          val x49 = x48
          val x178 = !x49
          val x53 = if x178 then {
            val x51 = x47
            val x181 = x51 < (x0.length)
            x181
          } else {
            false
          }
          x53
        } do {
          val x55 = x47
          val x184 = x55 + 1
          val x56 = x47 = x184
          val x57 = x47
          val x185 = x57 < (x0.length)
          val x63 = if x185 then {
            val x61 = {val x189 = '^' == (x0.charAt(x57))
            x189}
            x61
          } else {
            false
          }
          val x111 = if x63 then {
            val x192 = (x57 + 1) < (x0.length)
            val x69 = if x192 then {
              val x67 = {val x196 = 'h' == (x0.charAt((x57 + 1)))
              x196}
              x67
            } else {
              false
            }
            val x108 = if x69 then {
              val x199 = ((x57 + 1) + 1) < (x0.length)
              val x75 = if x199 then {
                val x73 = {val x203 = 'e' == (x0.charAt(((x57 + 1) + 1)))
                x203}
                x73
              } else {
                false
              }
              val x105 = if x75 then {
                val x206 = (((x57 + 1) + 1) + 1) < (x0.length)
                val x81 = if x206 then {
                  val x79 = {val x210 = 'l' == (x0.charAt((((x57 + 1) + 1) + 1)))
                  x210}
                  x79
                } else {
                  false
                }
                val x102 = if x81 then {
                  val x213 = ((((x57 + 1) + 1) + 1) + 1) < (x0.length)
                  val x87 = if x213 then {
                    val x85 = {val x217 = 'l' == (x0.charAt(((((x57 + 1) + 1) + 1) + 1)))
                    x217}
                    x85
                  } else {
                    false
                  }
                  val x99 = if x87 then {
                    val x220 = (((((x57 + 1) + 1) + 1) + 1) + 1) < (x0.length)
                    val x93 = if x220 then {
                      val x91 = {val x224 = 'o' == (x0.charAt((((((x57 + 1) + 1) + 1) + 1) + 1)))
                      x224}
                      x91
                    } else {
                      false
                    }
                    val x96 = if x93 then {
                      val x227 = ((((((x57 + 1) + 1) + 1) + 1) + 1) + 1) == (x0.length)
                      x227
                    } else {
                      false
                    }
                    x96
                  } else {
                    false
                  }
                  x99
                } else {
                  false
                }
                x102
              } else {
                false
              }
              x105
            } else {
              false
            }
            x108
          } else {
            false
          }
          val x112 = x48 = x111
          ()
        }
        
        val x115 = x48
        x115
      }
      x117
    }


}