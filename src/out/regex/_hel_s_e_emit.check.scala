def matcher(x: String): Boolean = {
  var r1 = -1
  var r2 = false
  while (!r2 && r1 < x.length) {
    r1 = r1 + 1
    var r3 = r1 + 1 + 1
    var r4 = r3 == x.length
    var r5 = false
    while (!r5 && !r4 && r3 < x.length) {
      r5 = !('l' == x.charAt(r3))
      r3 = r3 + 1
      r4 = r3 == x.length
    }
    r2 = r1 < x.length && 'h' == x.charAt(r1) && r1 + 1 < x.length && 'e' == x.charAt(r1 + 1) && !r5 && r4
  }
  r2
}
