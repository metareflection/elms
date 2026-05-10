package lms.test

// An adaptation of the original LMS's Shonan tutorial. This solves the HMM
// problem from the Shonan challenge.

import lms.prelude._
import lms.prelude.given
import lms.helpers.SnippetDriver
import lms.helpers.DslOps

@virtualize
class ShonanTest extends SnapshotFunSuite {
  val under = "shonan/"

  abstract class DslDriver[A: Typable, B: Typable]
    extends SnippetDriver[A, B] with DslOps

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
              for (j <- (0 `until` n): Range) { v1(i) = v1(i) + a(i)(j) * v(j) }
            } else {
              for (j <- (0 `until` n): Rep[Range]) { v1(i) = v1(i) + a(i)(j) * v(j) }
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
          // Because the argument type of `f` here is always `Rep[Int]`, this
          // won't properly inline the lookups to `a(i)`. This is solvable in
          // theory, but that's some very fiddly engineering effort for some
          // test code. Constant propagation on the IR should also work.
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
            for (j <- unrollIf(sparse, 0 `until` n)) { v1(i) = v1(i) + a(i)(j) * v(j) }
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
