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

  test("while scope") {
    object Snippet extends OptimizingSnippetDriver[Int, Int] with DslOps {
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
}
