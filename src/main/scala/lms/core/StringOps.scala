package lms.core

import lms.core.Op._

trait StringOps extends PrimitiveOps {
  lazy val stringLength: Rep[String => Int] = unsafeDeclare("String.length")
  lazy val stringTake: Rep[(String, Int) => String] = unsafeDeclare("String.take")
  lazy val stringDrop: Rep[(String, Int) => String] = unsafeDeclare("String.drop")
  lazy val stringStartsWith: Rep[(String, String) => Boolean] =
    unsafeDeclare("String.startsWith")
  lazy val stringEndsWith: Rep[(String, String) => Boolean] =
    unsafeDeclare("String.endsWith")
  lazy val stringCharAt: Rep[(String, Int) => Char] = unsafeDeclare("String.charAt")
  lazy val stringSubstring: Rep[(String, Int, Int) => String] =
    unsafeDeclare("String.substring")

  extension (s: Rep[String])
    def take(x: Rep[Int]): Rep[String] = stringTake(s, x)
    def drop(x: Rep[Int]): Rep[String] = stringDrop(s, x)
    def startsWith(haystack: Rep[String]): Rep[Boolean] = stringStartsWith(s, haystack)
    def endsWith(haystack: Rep[String]): Rep[Boolean] = stringEndsWith(s, haystack)
    def charAt(i: Rep[Int]): Rep[Char] = stringCharAt(s, i)
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String] =
      stringSubstring(s, st, end)

  given RepApply1[String, Int, Char] with
    def run(s: Rep[String], i: Rep[Int]): Rep[Char] = s.charAt(i)

  given RepLength[String] with
    def run(s: Rep[String]): Rep[Int] = stringLength(s)
}
