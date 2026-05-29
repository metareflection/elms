package elms.test

import scala.language.implicitConversions

import elms.prelude.{_, given}
import elms.helpers.SimpleSnippetDriver
import elms.helpers.DslOps

@virtualize
class VirtualizeTests extends SnapshotFunSuite {
  val under = "virtualize/"

  test("pow5") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
      def pow(x: Rep[Int], n: Int): Rep[Int] = if n == 0 then 1 else x * pow(x, n - 1)
      def snippet(x: Rep[Int]): Rep[Int] = pow(x, 5)
    }
    check("pow5", Snippet.code)
  }

  test("simple if") {
    object Snippet extends SimpleSnippetDriver[Boolean, Int] with DslOps {
      def snippet(x: Rep[Boolean]): Rep[Int] = if x then 1 else 0
    }
    check("if-basic", Snippet.code)
  }

  test("if nested") {
    object Snippet extends SimpleSnippetDriver[Boolean, Int] with DslOps {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        if x then { if x then 1 else 2 } else { 0 }
      }
    }
    check("if-nested", Snippet.code)
  }

  test("equality guard") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
      def snippet(x: Rep[Int]): Rep[Int] = { if (x === 1) 2 else x }
    }
    check("if-tutorial", Snippet.code)
  }

  test("pow-square") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
      def square(x: Rep[Int]): Rep[Int] = x * x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n / 2))
        else b * power(b, n - 1)

      def snippet(b: Rep[Int]): Rep[Int] = power(b, 7)
    }
    check("pow-square", Snippet.code)
  }

  test("function calls") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
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

  test("recursive functions") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
      def fact: Rep[Int => Int] = fun { (x: Rep[Int]) =>
        if x === 0 then unit(1) else snippet(x - 1)
      }
      def snippet(x: Rep[Int]) = fact(x)
    }
    check("fact", Snippet.code)
  }

  test("while") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
      def snippet(x: Rep[Int]): Rep[Int] = {
        while (x === 0) do Builtins.println(x)
        x
      }
    }
    check("while", Snippet.code)
  }

  test("vars") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
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
    check("var", Snippet.code)
  }

  test("if effects") {
    object Snippet extends SimpleSnippetDriver[Int, Int] with DslOps {
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
    check("if-guard-effects", Snippet.code)
  }

  test("start-1") {
    val snippet = new DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)

      }
    }
    check("1", snippet.code)
  }

  test("power") {
    val snippet = new DslDriver[Int,Int] {
      def square(x: Rep[Int]): Rep[Int] = x*x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n/2))
        else b * power(b, n-1)

      def snippet(b: Rep[Int]) =
        power(b, 7)

    }
    check("power", snippet.code)
  }

  test("range1") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = {
        for (i <- (0.until(3)): Range) {
          Builtins.println(i)
        }

      }
    }
    check("range1", snippet.code)
  }

  test("range2") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = {
        for (i <- (0.until(x)): Rep[Range]) {
          Builtins.println(i)
        }

      }
    }
    check("range2", snippet.code)
  }

  test("short-circuit") {
    val snippet = new SimpleSnippetDriver[Unit, Boolean] with DslOps {
      def foo(s: String, out: Boolean): Rep[Boolean] = {
        Builtins.println(s)
        unit(out)
      }

      def snippet(_x: Rep[Unit]): Rep[Boolean] = {
        val nab = foo("a", false) && foo("b", true)
        val anb = foo("a", true) || foo("b", false)

        nab && anb
      }
    }
    check("short-circuit", snippet.code)
  }
}
