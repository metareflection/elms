package lms

import scala.language.implicitConversions

import lms.prelude.{_, given}
import lms.core.Op.*
import lms.core.tree as ast
import lms.pipeline.eqsat.*
import lms.pipeline.simple
import lms.pipeline.Propagate
import lms.runtime.Log
import lms.util.Plumbing.*

import lms.helpers.{OptimizingDriver, DslOps}

import Pattern.{Var => PVar, Node => PNode}

@virtualize
trait Dsl extends DslOps {
  def fact: Rep[Int => Int] = fun { (x: Rep[Int]) =>
    if x === 0 then unit(1) else x * fact(x - 1)
  }
}

object Playground extends OptimizingDriver[Int, Int](Seq()) with Dsl {
  override val codegen = lms.codegen.CCodegen()

  def snippet(v: Rep[Int]): Rep[Int] = { fact(v) }

  override def extract() = {
    val prog = super.extract()
    ast.Program(
      prog.functions.map(_.mapRight(_.map(Propagate.run))),
      prog.staticData
    )
  }
}

@main
def main() = {
  println(Playground.code)
}

/*
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
    addcomm,
    addassoc,
    subnegate,
    sub,
    zero
  ))

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
  val xminusy = graph.addNode(Plus, Vector(x, minusy))
  val result = graph.addNode(Plus, Vector(y, xminusy))

  graph.saturate()

  println(graph.extract(result))
}
*/
