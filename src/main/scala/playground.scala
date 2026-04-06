package lms

import lms.core.Op.*
import lms.ir.opt.*
import lms.codegen.ast.*

import Pattern.{Var => PVar, Node => PNode}

@main
def main() = {
  val graph = new EGraph()

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
    addcomm,
    addassoc,
    subnegate,
    sub,
    zero
  )

  /*
    E(Plus, Vector(
      V("y"),
      E(Minus, Vector(V("x"), V("y")))
    ))
   */
  val x = graph.addVar("x")
  //val y = graph.addVar("y")
  //val minusy = graph.addNode(Negate, Vector(y))
  //val xminusy = graph.addNode(Plus, Vector(y, minusy))
  val z = graph.addNode(Const(0), Vector())
  val result = graph.addNode(Plus, Vector(x, z))

  graph.saturate(rules)

  println(graph.extract(result))
}
