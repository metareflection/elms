def matcher(x: String): Boolean = {
  0 < x.length && 'h' == x.charAt(0) && 0 + 1 < x.length && 'e' == x.charAt(0 + 1) && 0 + 1 + 1 < x.length && 'l' == x.charAt(0 + 1 + 1) && 0 + 1 + 1 + 1 < x.length && 'l' == x.charAt(0 + 1 + 1 + 1) && 0 + 1 + 1 + 1 + 1 < x.length && 'o' == x.charAt(0 + 1 + 1 + 1 + 1) && 0 + 1 + 1 + 1 + 1 + 1 == x.length
}
