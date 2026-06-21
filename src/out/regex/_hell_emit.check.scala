def matcher(x: String): Boolean = {
  var r1 = -1
  var r2 = false
  while (!r2 && r1 < x.length) {
    r1 = r1 + 1
    r2 = r1 < x.length && 'h' == x.charAt(r1) && r1 + 1 < x.length && 'e' == x.charAt(r1 + 1) && r1 + 1 + 1 < x.length && 'l' == x.charAt(r1 + 1 + 1) && r1 + 1 + 1 + 1 < x.length && 'l' == x.charAt(r1 + 1 + 1 + 1)
  }
  r2
}
