package elms

import scala.language.implicitConversions

import elms.prelude.{_, given}
import elms.core.Op.*
import elms.core.tree as ast
import elms.pipeline.eqsat.*
import elms.pipeline.simple
import elms.pipeline.Propagate
import elms.runtime.Log
import elms.util.Plumbing.*

import Pattern.{Var => PVar, Node => PNode}

@virtualize
trait Ackermann extends DslOps {
  def a(m: Int): Rep[Int => Int] = fun { (n: Rep[Int]) =>
    if m == 0 then n + 1 else if n === 0 then a(m - 1)(1) else a(m - 1)(a(m)(n - 1))
  }
}

@virtualize
trait Fact extends DslOps {
  def fact: Rep[Int => Int] = fun { (n: Rep[Int]) =>
    if n === 0 then unit(1) else n * fact(n-1)
  }
}

@main
def main() = {
  /*
  def specialize(m: Int): OptimizingSnippetDriver[Int, Int] =
    new OptimizingSnippetDriver[Int, Int] with Ackermann {
      def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
    }

  val ack2 = specialize(2)
  println(ack2.code)
  */
  println(new OptimizingSnippetDriver[Int, Int] with Fact {
    def snippet(n: Rep[Int]): Rep[Int] = fact(n)
  }.code)
}

/*
@main
def main() = {
  elms.runtime.Log = elms.util.Logger.debug

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
