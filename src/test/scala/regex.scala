package elms.test

import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given

@virtualize
class RegexTest extends SnapshotFunSuite {
  val under = "regex/"

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
      }
      else if (start < text.length && matchchar(regexp.charAt(restart), text(start))) then
        matchhere(regexp, restart + 1, text, start + 1)
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

    def matchchar(c: Char, t: Rep[Char]): Rep[Boolean] = { c == '.' || c === t }
  }
}
