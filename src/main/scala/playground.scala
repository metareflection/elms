package lms

import lms.prelude.{_, given}
import lms.core.Op.*
import lms.ir.eqsat.*
import lms.codegen.ast.*

import lms.helpers.{SnippetDriver, DslOps}

import Pattern.{Var => PVar, Node => PNode}

trait Dsl extends DslOps {
  @virtualize
  def f(x: Rep[String], n: Int): Rep[Int] = { length(x) + n }
}

@virtualize
object Playground
    extends SnippetDriver[Array[Int], Array[Int]](irBuilder =
      Builder(Builder.Config(Seq(), EGraph.Config()))
    )
    with DslOps {
  val A = scala.Array

  val a = A(
    A(1, 1, 1, 1, 1), // dense
    A(0, 0, 0, 0, 0), // null
    A(0, 0, 1, 0, 0), // sparse
    A(0, 0, 0, 0, 0),
    A(0, 0, 1, 0, 1)
  )

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

@main
def main() = {
  println(Playground.code)
}

/*
@main
def main() = {
  val addcomm = Equivalence(
    PNode(Plus, Vector(PVar("x"), PVar("y"))),
    PNode(Plus, Vector(PVar("y"), PVar("x")))
  )
  val addassoc = Equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Plus, Vector(PVar("y"), PVar("z"))))),
    PNode(Plus, Vector(PNode(Plus, Vector(PVar("x"), PVar("y"))), PVar("z")))
  )
  val subnegate = Equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Negate, Vector(PVar("y"))))),
    PNode(Minus, Vector(PVar("x"), PVar("y")))
  )
  val sub = Rewrite(PNode(Minus, Vector(PVar("x"), PVar("x"))), PNode(Const(0), Vector()))
  val zero = Rewrite(PNode(Plus, Vector(PVar("x"), PNode(Const(0), Vector()))), PVar("x"))

  val rules = Seq(
    //addcomm,
    addassoc,
    subnegate,
    sub,
    zero
  )

  val graph = new EGraph(rules)

  /*
    E(Plus, Vector(
      V("y"),
      E(Minus, Vector(V("x"), V("y")))
    ))
 */
  val x = graph.addNamedVar("x")
  val y = graph.addNamedVar("y")
  val minusy = graph.addNode(Negate, Vector(y))
  val yminusy = graph.addNode(Plus, Vector(y, minusy))
  val result = graph.addNode(Plus, Vector(x, yminusy))

  graph.saturate()

  println(graph.extract(result))
}
 */
