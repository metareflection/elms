package lms

import scala.language.implicitConversions

import lms.prelude.{_, given}
import lms.core.Op.*
import lms.pipeline.eqsat.*
import lms.pipeline.simple
import lms.pipeline.tree as ast
import lms.runtime.Log

import lms.helpers.{SnippetDriver, DslOps}

import Pattern.{Var => PVar, Node => PNode}

@main
def main() = {
  lms.runtime.Log = lms.util.Logger.debug

  val addcomm = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PVar("y"))),
    PNode(Plus, Vector(PVar("y"), PVar("x")))
  )
  val addassoc = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Plus, Vector(PVar("y"), PVar("z"))))),
    PNode(Plus, Vector(PNode(Plus, Vector(PVar("x"), PVar("y"))), PVar("z")))
  )
  val subnegate = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Negate, Vector(PVar("y"))))),
    PNode(Minus, Vector(PVar("x"), PVar("y")))
  )
  val sub = Rule.rewrite(PNode(Minus, Vector(PVar("x"), PVar("x"))), PNode(Const(0), Vector()))
  val zero = Rule.rewrite(PNode(Plus, Vector(PVar("x"), PNode(Const(0), Vector()))), PVar("x"))

  val rules = Ruleset(Seq(
    //addcomm,
    //addassoc,
    //subnegate,
    //sub,
    zero
  ))

  val graph = new EGraph(rules)

  /*
    E(Plus, Vector(
      V("y"),
      E(Minus, Vector(V("x"), V("y")))
    ))
  */
  /*
  val x = graph.addNamedVar("x")
  val y = graph.addNamedVar("y")
  val minusy = graph.addNode(Negate, Vector(y))
  val xminusy = graph.addNode(Plus, Vector(x, minusy))
  val result = graph.addNode(Plus, Vector(y, xminusy))
  */
  val x = graph.addNamedVar("x")
  val y = graph.addNamedVar("y")
  //val minusx = graph.addNode(Negate, Vector(x))
  //val xminusx = graph.addNode(Plus, Vector(x, minusx))
  val result = graph.addNode(Plus, Vector(y, graph.addNode(Const(0), Vector())))
  //val minusx = graph.addNode(Negate, Vector(x))
  //val result = graph.addNode(Plus, Vector(x, minusx))

  graph.saturate()

  println(graph.extract(result))
}
