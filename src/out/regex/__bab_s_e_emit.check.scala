def matcher(x: String): Boolean = {
  var r1 = 0 + 1
  var r2 = r1 == x.length
  var r3 = false
  while (!r3 && !r2 && r1 < x.length) {
    r3 = !('b' == x.charAt(r1))
    r1 = r1 + 1
    r2 = r1 == x.length
  }
  0 < x.length && 'a' == x.charAt(0) && !r3 && r2
}
