package elms.test

import scala.language.implicitConversions

import elms.prelude.{_, given}
import elms.helpers.OptimizingSnippetDriver
import elms.helpers.DslOps

@virtualize
class OptimizerTests extends SnapshotFunSuite {
  val under = "opts/"

  // These two tests are intended to ensure that scoped elaboration correctly
  // respects scopes. For example,
  //
  // if {
  //   val x = 1
  //   x == 2
  // } {
  //   // ...
  // }
  //
  // does not expose `x`.

  test("if scope") {
    object Snippet extends DslDriver[Int, Int] {
      def snippet(x: Rep[Int]): Rep[Int] = {
        val y = newVar(x)
        val result = newVar(0)
        if {
          y := y.get + 2
          y.get > 0
        } then {
          result := result.get + y.get
        }

        result.get
      }
    }
    check("if-scope", Snippet.code)
  }

  test("while scope") {
    object Snippet extends DslDriver[Int, Int] {
      def snippet(x: Rep[Int]): Rep[Int] = {
        val y = newVar(x)
        val result = newVar(0)
        while y.get > 0 do {
          result := result.get + y.get
          y := y.get - 1
        }

        result.get
      }
    }
    check("while-scope", Snippet.code)
  }

  // Ensure that boolean operators respect short-circuiting

  test("short-circuit") {
    val snippet = new DslDriver[Boolean, Boolean] {
      def foo(s: String, out: Boolean): Rep[Boolean] = {
        Builtins.println(s)
        unit(out)
      }

      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        val nab = foo("a", false) && foo("b", true)
        val anb = foo("a", true) || foo("b", false)

        (x || nab) || (x && anb)
      }
    }
    check("short-circuit", snippet.code)
  }
}
