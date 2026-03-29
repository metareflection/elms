package lms.test

// An adaptation of the original LMS's Shonan tutorial. This solves the HMM
// problem from the Shonan challenge.

import lms.prelude._
import lms.helpers.SnippetDriver
import lms.helpers.DslOps

@virtualize
class ShonanTest extends SnapshotFunSuite {
  val under = "shonan/"

  val A = scala.Array

  val a =
    A(A(1, 1, 1, 1, 1), // dense
      A(0, 0, 0, 0, 0), // null
      A(0, 0, 1, 0, 0), // sparse
      A(0, 0, 0, 0, 0),
      A(0, 0, 1, 0, 1))

  test("shonan-hmm1a") {
    def matrix_vector_prod(a: Array[Array[Int]], v: Array[Int]) = {
      val n = a.length
      val v1 = new Array[Int](n)

      for (i <- (0 until n)) {
        for (j <- (0 until n)) {
          v1(i) = v1(i) + a(i)(j) * v(j)
        }
      }
      v1
    }

    val v = A(1,1,1,1,1)

    val v1 = matrix_vector_prod(a, v)
    val result = v1.mkString(",")

    check("hmm1a", result)
  }

  test("shonan-hmm1b") {
    object Snippet extends SnippetDriver[Array[Int],Array[Int]] {
      def snippet(v: Rep[Array[Int]]) = {

        val x = 100 - 5

        if (x > 10) {
          println("hello")
        }

        v
      }
    }
    check("hmm1b", Snippet.code)
  }
}
