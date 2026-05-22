package elms.test.tutorial

import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given

import elms.test.*

@virtualize
class GettingStartedTest extends SnapshotFunSuite {
  val under = "tutorial/dslapi-"

  test("1") {
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
        for (i <- (0 until 3): Range) {
          Builtins.println(i)
        }

      }
    }
    check("range1", snippet.code)
  }

  test("range2") {
    val snippet = new DslDriver[Int,Unit] {
      def snippet(x: Rep[Int]) = {
        for (i <- (0 until x): Rep[Range]) {
          Builtins.println(i)
        }

      }
    }
    check("range2", snippet.code)
  }
}
