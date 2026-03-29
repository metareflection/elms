package lms.core

import lms.core.Op._
import lms.util.OverloadHack._

trait StringOps extends PrimitiveOps {
  val stringLength: Rep[String => Int] = unsafeDeclare("String.length")
  val stringTake: Rep[(String, Int) => String] = unsafeDeclare("String.take")
  val stringDrop: Rep[(String, Int) => String] = unsafeDeclare("String.drop")
  val stringStartsWith: Rep[(String, String) => Boolean] =
    unsafeDeclare("String.startsWith")
  val stringEndsWith: Rep[(String, String) => Boolean] =
    unsafeDeclare("String.endsWith")
  val stringCharAt: Rep[(String, Int) => Char] = unsafeDeclare("String.charAt")
  val stringSubstring: Rep[(String, Int, Int) => String] =
    unsafeDeclare("String.substring")

  extension (s: Rep[String])
    def length(): Rep[Int] = stringLength(s)
    def take(x: Rep[Int]): Rep[String] = stringTake(s, x)
    def drop(x: Rep[Int]): Rep[String] = stringDrop(s, x)
    def startsWith(haystack: Rep[String]): Rep[Boolean] = stringStartsWith(s, haystack)
    def endsWith(haystack: Rep[String]): Rep[Boolean] = stringEndsWith(s, haystack)
    def charAt(i: Rep[Int]): Rep[Char] = stringCharAt(s, i)
    def apply(i: Rep[Int])(using O2): Rep[Char] = s.charAt(i)
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String] =
      stringSubstring(s, st, end)
}
