package elms.test

import scala.language.implicitConversions

// An adaptation of the original LMS's Shonan tutorial. This solves the HMM
// problem from the Shonan challenge.

import elms.prelude._
import elms.prelude.given
import elms.helpers.OptimizingSnippetDriver
import elms.helpers.DslOps

@virtualize
class ShonanTest extends SnapshotFunSuite {
  val under = "shonan/"

  abstract class DslDriver[A: Typable, B: Typable]
    extends OptimizingSnippetDriver[A, B](Seq()) with DslOps

  val A = scala.Array

  val a = A(
    A(1, 1, 1, 1, 1), // dense
    A(0, 0, 0, 0, 0), // null
    A(0, 0, 1, 0, 0), // sparse
    A(0, 0, 0, 0, 0),
    A(0, 0, 1, 0, 1)
  )

  test("shonan-hmm1a") {
    def matrix_vector_prod(a: Array[Array[Int]], v: Array[Int]) = {
      val n = a.length
      val v1 = new Array[Int](n)

      for (i <- (0 until n)) {
        for (j <- (0 until n)) { v1(i) = v1(i) + a(i)(j) * v(j) }
      }
      v1
    }

    val v = A(1, 1, 1, 1, 1)

    val v1 = matrix_vector_prod(a, v)
    val result = v1.mkString(",")

    check("hmm1a", result)
  }

  test("shonan-hmm1b") {
    object Snippet extends DslDriver[Array[Int], Array[Int]] with java.io.Serializable {
      def snippet(v: Rep[Array[Int]]) = {

        val x = 100 - 5

        if (x > 10) { Builtins.println(unit("hello")) }

        v
      }
    }
    check("hmm1b", Snippet.code)
  }

  test("shonan-hmm1b_dyn") {
    object Snippet extends DslDriver[Array[Int], Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        val x = 100 - v.length

        if (x > 10) { Builtins.println("hello") }

        v
      }
    }

    check("shonan-hmm1b_dyn", Snippet.code)
  }

  test("shonan-hmm1c") {
    object Snippet extends DslDriver[Array[Int], Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        def matrix_vector_prod(a0: Array[Array[Int]], v: Rep[Array[Int]]) = {
          val n = a0.length
          val v1 = newArray[Int](n)

          for (i <- (0 `until` n): Range) {
            val sparse = a0(i).count(_ != 0) < 3
            if (sparse) {
              for (j <- (0 `until` n): Range) { v1(i) = v1(i) + a0(i)(j) * v(j) }
            } else {
              val ai = staticData(a0(i))
              for (j <- (0 `until` n): Rep[Range]) { v1(i) = v1(i) + ai(j) * v(j) }
            }
          }
          v1
        }

        val v1 = matrix_vector_prod(a, v)
        v1
      }
    }
    check("shonan-hmm1c", Snippet.code)
  }

  test("shonan-hmm1d") {
    object Snippet extends DslDriver[Array[Int], Array[Int]] {
      // XXX: We could avoid needing to define this if we were a bit less
      // cute with `unrollIf` below.
      trait UnrollIf {
        def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit]
      }

      def snippet(v: Rep[Array[Int]]) = {

        def unrollIf(sparse: Boolean, r: Range) = new UnrollIf {
          def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
            if (sparse) for (j <- (r.start `until` r.end): Range) f(j)
            else for (j <- (r.start `until` r.end): Rep[Range]) f(j)
          }
        }

        def matrix_vector_prod(a0: Array[Array[Int]], v: Rep[Array[Int]]) = {
          val n = a0.length
          val v1 = newArray[Int](n)

          for (i <- (0 `until` n): Range) {
            val sparse = a0(i).count(_ != 0) < 3
            val ai = staticData(a0(i))
            for (j <- unrollIf(sparse, 0 `until` n)) { v1(i) = v1(i) + ai(j) * v(j) }
          }
          v1
        }

        val v1 = matrix_vector_prod(a, v)
        v1
      }
    }

    check("shonan-hmm1d", Snippet.code)
  }
}
