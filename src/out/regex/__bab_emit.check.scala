def matcher(x: String): Boolean = {
  0 < x.length && 'a' == x.charAt(0) && 0 + 1 < x.length && 'b' == x.charAt(0 + 1)
}
