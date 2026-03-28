package lms.core

import lms.core.Op._
import lms.util.OverloadHack._

trait StringOps extends Base {
  extension (s: Rep[String])
    def length(): Rep[Int] = unsafeReflect(StringLength, s)
    def take(x: Rep[Int]): Rep[String] = unsafeReflect(StringTake, s, x)
    def drop(x: Rep[Int]): Rep[String] = unsafeReflect(StringDrop, s, x)
    def startsWith(haystack: Rep[String]): Rep[Boolean] =
      unsafeReflect(StringStartsWith, s, haystack)
    def endsWith(haystack: Rep[String]): Rep[Boolean] =
      unsafeReflect(StringEndsWith, s, haystack)
    def charAt(i: Rep[Int]): Rep[Char] = unsafeReflect(StringCharAt, s, i)
    def apply(i: Rep[Int])(using o: O2): Rep[Char] = s.charAt(i)
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String] =
      unsafeReflect(StringSubstring, s, st, end)
}
