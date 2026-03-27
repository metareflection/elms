package lms.test

import lms.prelude._
import lms.helpers.SnippetDriver

@virtualize
class VirtualizeTests extends SnapshotFunSuite {
  val under = "virtualize/"

  test("pow5") {
    object Snippet extends SnippetDriver[Int, Int] with PrimitiveOps {
      def pow(x: Rep[Int], n: Int): Rep[Int] = if n == 0 then 1 else x * pow(x, n - 1)
      def snippet(x: Rep[Int]): Rep[Int] = pow(x, 5)
    }

    check("pow5", Snippet.code)
  }

  test("simple if") {
    object Snippet extends SnippetDriver[Boolean, Int] with PrimitiveOps {
      def snippet(x: Rep[Boolean]): Rep[Int] = if x then 1 else 0
    }

    check("if-basic", Snippet.code)
  }

  test("if nested") {
    object Snippet extends SnippetDriver[Boolean, Int] with PrimitiveOps {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        if x then {
          if x then 1 else 2
        } else {
          0
        }
      }
    }

    check("if-nested", Snippet.code)
  }

  test("equality guard") {
    object Snippet extends SnippetDriver[Int, Int] with PrimitiveOps {
      def snippet(x: Rep[Int]): Rep[Int] = {
        if (x === 1) 2 else x
      }
    }
    check("if-tutorial", Snippet.code)
  }

  test("pow-square") {
    object Snippet extends SnippetDriver[Int,Int] with PrimitiveOps {
      def square(x: Rep[Int]): Rep[Int] = x*x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n/2))
        else b * power(b, n-1)

      def snippet(b: Rep[Int]): Rep[Int] =
        power(b, 7)
    }
    check("pow-square", Snippet.code, accept=true)
  }

  test("function calls") {
    object Snippet extends SnippetDriver[Int, Int] with PrimitiveOps {
      def snippet(x: Rep[Int]) = {
        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x === 1)
      }
    }
    check("func-tutorial", Snippet.code)
  }
}
