package elms.test

import scala.collection.mutable
import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given

class RegexTest extends SnapshotFunSuite {
  val under = "regex/"

  object Matcher {
    /* search for regexp anywhere in text */
    def matchsearch(regexp: String, text: String): Boolean = {
      if (regexp(0) == '^') matchhere(regexp, 1, text, 0)
      else {
        var start = -1
        var found = false
        while (!found && start < text.length) {
          start += 1
          found = matchhere(regexp, 0, text, start)
        }
        found
      }
    }

    /* search for restart of regexp at start of text */
    def matchhere(regexp: String, restart: Int, text: String, start: Int): Boolean = {
      if (restart == regexp.length) true
      else if (regexp(restart) == '$' && restart + 1 == regexp.length) start ==
        text.length
      else if (restart + 1 < regexp.length && regexp(restart + 1) == '*')
        matchstar(regexp(restart), regexp, restart + 2, text, start)
      else if (start < text.length && matchchar(regexp(restart), text(start)))
        matchhere(regexp, restart + 1, text, start + 1)
      else false
    }

    /* search for c* followed by restart of regexp at start of text */
    def matchstar(
        c: Char,
        regexp: String,
        restart: Int,
        text: String,
        start: Int
    ): Boolean = {
      var sstart = start
      var found = matchhere(regexp, restart, text, sstart)
      var failed = false
      while (!failed && !found && sstart < text.length) {
        failed = !matchchar(c, text(sstart))
        sstart += 1
        found = matchhere(regexp, restart, text, sstart)
      }
      !failed && found
    }

    def matchchar(c: Char, t: Char): Boolean = { c == '.' || c == t }
  }

  @virtualize
  trait Ops extends DslOps {
    /* search for regexp anywhere in text */
    def matchsearch(regexp: String, text: Rep[String]): Rep[Boolean] = {
      if (regexp(0) === '^') matchhere(regexp, 1, text, 0)
      else {
        val start = newVar(-1)
        val found = newVar(false)
        while (!found && start < text.length) {
          start := start + 1
          found := matchhere(regexp, 0, text, start)
        }
        found
      }
    }

    /* search for restart of regexp at start of text */
    def matchhere(
        regexp: String,
        restart: Int,
        text: Rep[String],
        start: Rep[Int]
    ): Rep[Boolean] = {
      if restart == regexp.length then true
      else if (regexp.charAt(restart) == '$' && restart + 1 == regexp.length) then
        start === text.length
      else if (restart + 1 < regexp.length && regexp.charAt(restart + 1) == '*') then {
        matchstar(regexp.charAt(restart), regexp, restart + 2, text, start)
      } else if (start < text.length && matchchar(regexp.charAt(restart), text(start)))
      then matchhere(regexp, restart + 1, text, start + 1)
      else false
    }

    /* search for c* followed by restart of regexp at start of text */
    def matchstar(
        c: Char,
        regexp: String,
        restart: Int,
        text: Rep[String],
        start: Rep[Int]
    ): Rep[Boolean] = {
      val sstart = newVar(start)
      val found = newVar(matchhere(regexp, restart, text, sstart))
      val failed = newVar(false)
      while (!failed && !found && sstart < text.length) {
        failed := !matchchar(c, text(sstart))
        sstart := sstart + 1
        found := matchhere(regexp, restart, text, sstart)
      }
      !failed && found
    }

    def matchchar(c: Char, t: Rep[Char]): Rep[Boolean] = unit(c == '.') || c === t
  }

  type RegexDriver = DslDriver[String, Boolean] & EvalScalaSnippet[String, Boolean]

  val cache = mutable.Map[String, RegexDriver]()

  def sanitize(regexp: String): String =
    "_" + regexp.replace("^", "_b").replace("*", "_s").replace("$", "_e")

  def getOrBuild(regexp: String): RegexDriver =
    cache.get(regexp).getOrElse {
      cache(regexp) = new DslDriver[String, Boolean]
          with Ops
          with EvalScalaSnippet[String, Boolean] {
        val prefix = "regex-test"
        val name = sanitize(regexp)
        def snippet(x: Rep[String]) = matchsearch(regexp, x)
      }
      cache(regexp)
    }

  def testmatch(regexp: String, text: String, expected: Boolean) = {
    test(s"""matchsearch("$regexp", "$text") == $expected""") {
      val snippet = getOrBuild(regexp)
      assertResult(expected) { snippet.eval(classOf[String], text) }
      check(sanitize(regexp), snippet.code)
    }
  }

  testmatch("^hello$", "hello", true)
  testmatch("^hello$", "hell", false)
  testmatch("hell", "hello", true)
  testmatch("hell", "hell", true)
  testmatch("hel*", "he", true)
  testmatch("hel*$", "hello", false)
  testmatch("hel*", "yo hello", true)
  testmatch("ab", "hello ab hello", true)
  testmatch("^ab", "hello ab hello", false)
  testmatch("a*b", "hello aab hello", true)
  testmatch("^ab*", "abcd", true)
  testmatch("^ab*", "a", true)
  testmatch("^ab*", "ac", true)
  testmatch("^ab*", "bac", false)
  testmatch("^ab*$", "ac", false)
}
