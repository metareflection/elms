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
      else if (regexp(restart) == '$' && restart + 1 == regexp.length)
        start == text.length
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
  trait StagedMatcher extends DslOps {
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
      else if (restart + 1 < regexp.length && regexp.charAt(restart + 1) == '*') then
        matchstar(regexp.charAt(restart), regexp, restart + 2, text, start)
      else if (start < text.length && matchchar(regexp.charAt(restart), text(start)))
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

  /* A third implementation, between the interpreter (`Matcher`) and the staged
   * generator (`StagedMatcher`). Staging turned the interpreter into a code generator
   * almost for free. Here we instead write a compiler by emitting code. */
  class CompilerEmitter {
    private val out = new StringBuilder
    private var n = 0
    private def fresh(): String = { n += 1; s"r$n" }
    private def emit(line: String): Unit = out.append(line).append('\n')

    // "" stands for the always-true fragment, so chains fold cleanly.
    private def and(a: String, b: String): String =
      if (a.isEmpty) b else if (b.isEmpty) a else s"$a && $b"
    private def orTrue(a: String): String = if (a.isEmpty) "true" else a

    def matchchar(c: Char, t: String): String =
      if (c == '.') "" else s"'$c' == $t"

    def matchhere(regexp: String, restart: Int, start: String): String =
      if (restart == regexp.length) ""
      else if (regexp(restart) == '$' && restart + 1 == regexp.length)
        s"$start == x.length"
      else if (restart + 1 < regexp.length && regexp(restart + 1) == '*')
        matchstar(regexp(restart), regexp, restart + 2, start)
      else
        and(
          and(s"$start < x.length", matchchar(regexp(restart), s"x.charAt($start)")),
          matchhere(regexp, restart + 1, s"$start + 1")
        )

    def matchstar(c: Char, regexp: String, restart: Int, start: String): String = {
      val sstart = fresh(); val found = fresh(); val failed = fresh()
      emit(s"var $sstart = $start")
      emit(s"var $found = ${orTrue(matchhere(regexp, restart, sstart))}")
      emit(s"var $failed = false")
      emit(s"while (!$failed && !$found && $sstart < x.length) {")
      emit(s"$failed = !(${orTrue(matchchar(c, s"x.charAt($sstart)"))})")
      emit(s"$sstart = $sstart + 1")
      emit(s"$found = ${orTrue(matchhere(regexp, restart, sstart))}")
      emit("}")
      s"!$failed && $found"
    }

    def matchsearch(regexp: String): String =
      if (regexp(0) == '^') matchhere(regexp, 1, "0")
      else {
        val pos = fresh(); val found = fresh()
        emit(s"var $pos = -1")
        emit(s"var $found = false")
        emit(s"while (!$found && $pos < x.length) {")
        emit(s"$pos = $pos + 1")
        emit(s"$found = ${orTrue(matchhere(regexp, 0, pos))}")
        emit("}")
        found
      }

    def compile(regexp: String): String = {
      val result = orTrue(matchsearch(regexp))
      s"def matcher(x: String): Boolean = {\n${reindent(out.toString)}  $result\n}\n"
    }

    private def reindent(src: String): String = {
      var depth = 1
      val sb = new StringBuilder
      for line <- src.linesIterator do {
        val t = line.trim
        if (t.startsWith("}")) depth -= 1
        sb.append("  " * depth).append(t).append('\n')
        if (t.endsWith("{")) depth += 1
      }
      sb.toString
    }
  }

  type RegexDriver = DslDriver[String, Boolean] & EvalScalaSnippet[String, Boolean]

  val cache = mutable.Map[String, RegexDriver]()

  def sanitize(regexp: String): String =
    "_" + regexp.replace("^", "_b").replace("*", "_s").replace("$", "_e")

  def getOrBuild(regexp: String): RegexDriver =
    cache.get(regexp).getOrElse {
      cache(regexp) = new DslDriver[String, Boolean]
          with StagedMatcher
          with EvalScalaSnippet[String, Boolean] {
        val prefix = "regex-test"
        val name = sanitize(regexp)
        def snippet(x: Rep[String]) = matchsearch(regexp, x)
      }
      cache(regexp)
    }

  def testmatch_interp(regexp: String, text: String, expected: Boolean, headline: String) = {
    test("interp_"+headline) {
      assertResult(expected) { Matcher.matchsearch(regexp, text) }
    }
  }

  def testmatch_staged(regexp: String, text: String, expected: Boolean, headline: String) = {
    test("staged_"+headline) {
      val snippet = getOrBuild(regexp)
      assertResult(expected) { snippet.eval(text) }
      check(sanitize(regexp), snippet.code)
    }
  }

  // Compile the emitted matcher with scalac, load it, and run it
  def evalEmitted(name: String, code: String, text: String): Boolean = {
    import java.nio.file.Files
    import java.net.URLClassLoader
    import dotty.tools.dotc.Main
    val src = s"object $name {\n  $code\n  def snippet(x: String): Boolean = matcher(x)\n}\n"
    val root = Files.createTempDirectory("regex-emit")
    val out = root.resolve("out")
    Files.createDirectories(out)
    val file = root.resolve(s"$name.scala")
    Files.writeString(file, src)
    val cp = sys.props("generated.test.classpath")
    val reporter = Main.process(Array("-classpath", cp, "-d", out.toString, file.toString))
    if reporter.hasErrors then
      throw new RuntimeException(s"failed to compile emitted matcher `$name`")
    val cls = new URLClassLoader(Array(out.toUri.toURL), getClass.getClassLoader)
      .loadClass(s"$name$$")
    val snippet = cls.getMethods.find(_.getName == "snippet").get
    snippet.invoke(cls.getField("MODULE$").get(null), text).asInstanceOf[Boolean]
  }

  def testmatch_emit(regexp: String, text: String, expected: Boolean, headline: String) = {
    test("emitter_"+headline) {
      val name = sanitize(regexp) + "_emit"
      val code = new CompilerEmitter().compile(regexp)
      assertResult(expected) { evalEmitted(name, code, text) }
      check(name, code)
    }
  }

  def testall(regexp: String, text: String, expected: Boolean) = {
    val headline = s"""matchsearch("$regexp", "$text") == $expected""""
    testmatch_interp(regexp, text, expected, headline)
    testmatch_staged(regexp, text, expected, headline)
    testmatch_emit(regexp, text, expected, headline)
  }

  testall("^hello$", "hello", true)
  testall("^hello$", "hell", false)
  testall("hell", "hello", true)
  testall("hell", "hell", true)
  testall("hel*", "he", true)
  testall("hel*$", "hello", false)
  testall("hel*", "yo hello", true)
  testall("ab", "hello ab hello", true)
  testall("^ab", "hello ab hello", false)
  testall("a*b", "hello aab hello", true)
  testall("^ab*", "abcd", true)
  testall("^ab*", "a", true)
  testall("^ab*", "ac", true)
  testall("^ab*", "bac", false)
  testall("^ab*$", "ac", false)
}
