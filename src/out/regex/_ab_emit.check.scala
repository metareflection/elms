def matcher(x: String): Boolean = {
  var r1 = -1
  var r2 = false
  while (!r2 && r1 < x.length) {
    r1 = r1 + 1
    r2 = r1 < x.length && 'a' == x.charAt(r1) && r1 + 1 < x.length && 'b' == x.charAt(r1 + 1)
  }
  r2
}
