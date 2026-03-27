package lms.test

import lms.prelude._
import lms.core.{StringOps, BooleanOps}
import lms.helpers.SnippetDriver

sealed abstract class Regex {}

case class Void() extends Regex
case class Empty() extends Regex
case class Lit(s: String) extends Regex

case class Concat(l: Regex, r: Regex) extends Regex
case class Or(l: Regex, r: Regex) extends Regex
case class Star(r: Regex) extends Regex

def check(regex: Regex, input: String, k: String => Boolean): Boolean = {
  regex match {
    case Void()       => false
    case Empty()      => k(input)
    case Lit(s)       => input.startsWith(s) && k(input.drop(s.length))
    case Concat(l, r) => check(l, input, check(r, _, k))
    case Or(l, r)     => check(l, input, k) || check(r, input, k)
    case Star(r)      => check(r, input, check(Star(r), _, k))
  }
}

@virtualize
class RegexMatcher extends SnapshotFunSuite {
  val under = "regex/"

  object Snippet extends SnippetDriver[String, Boolean] with StringOps with BooleanOps {
    def check(
        regex: Regex,
        input: Rep[String],
        k: Rep[String] => Rep[Boolean]
    ): Rep[Boolean] = {
      regex match {
        case Void()  => false
        case Empty() => k(input)
        case Lit(s)  => input.startsWith(s) && k(input.drop(s.length))
        case Concat(l, r) => check(l, input, check(r, _, k))
        case Or(l, r)     => check(l, input, k) || check(r, input, k)
        case Star(r)      => check(r, input, check(Star(r), _, k))
      }
    }

    // (0|(1(01*0)*1))*
    val z = Lit("0")
    val sz = Lit("1")
    val r = Or(z, Concat(Concat(sz, Star(Concat(z, Concat(Star(sz), z)))), sz))

    def snippet(input: Rep[String]): Rep[Boolean] = {
      check(r, input, (s: Rep[String]) => s.length() === unit(0))
    }
  }

  test("regex-cont") { check("regex-cont", Snippet.code) }
}
