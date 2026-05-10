package lms

import lms.prelude.{_, given}
import lms.core.Op.*
import lms.ir.eqsat.*
import lms.ir.simple
import lms.codegen.ast.*

import lms.helpers.{SnippetDriver, DslOps}

import Pattern.{Var => PVar, Node => PNode}

@virtualize
object Playground
    extends SnippetDriver[Int, Int](irBuilder =
      Builder(Builder.Config(Seq(), EGraph.Config()))
    )
    with DslOps {

  def fact: Rep[Int => Int] = fun { (x: Rep[Int]) =>
    if x === 0 then unit(1) else x * fact(x-1)
  }

  def snippet(v: Rep[Int]): Rep[Int] = {
    fact(v)
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
