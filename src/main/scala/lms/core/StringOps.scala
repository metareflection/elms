package elms.core

import elms.core.Op._

trait StringOps extends PrimitiveOps {
  extension (s: Rep[String])
    def take(x: Rep[Int]): Rep[String] = unsafeReflect(StringTake, s, x)
    def drop(x: Rep[Int]): Rep[String] = unsafeReflect(StringDrop, s, x)
    def startsWith(haystack: Rep[String]): Rep[Boolean] =
      unsafeReflect(StringStartsWith, s, haystack)
    def endsWith(haystack: Rep[String]): Rep[Boolean] =
      unsafeReflect(StringEndsWith, s, haystack)
    def charAt(i: Rep[Int]): Rep[Char] = unsafeReflect(StringCharAt, s, i)
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String] =
      unsafeReflect(StringSubstring, s, st, end)

  given RepApply1[String, Int, Char] with
    def run(s: Rep[String], i: Rep[Int]): Rep[Char] = s.charAt(i)

  given RepLength[String] with
    def run(s: Rep[String]): Rep[Int] = unsafeReflect(StringLength, s)
}
