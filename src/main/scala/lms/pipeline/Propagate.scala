package elms.pipeline

import elms.core.{Name, Primitive}
import elms.core.given
import elms.core.Op
import elms.core.tree.*

object Propagate {
  sealed trait Fact

  case class FConst[A: Primitive](c: Op.Const[A]) extends Fact
  case class FCopy(of: Name) extends Fact

  def run(t: Term): Term = propagateImpl(t, Map.empty)

  def propagateImpl(t: Term, facts: Map[Name, Fact]): Term = t match {
    case Let(x, e1, e2) => {
      val t1 = propagateImpl(e1, facts)
      classify(t1, facts) match {
        case Some(fact) => propagateImpl(e2, facts + (x -> fact))
        case None       => Let(x, t1, propagateImpl(e2, facts))
      }
    }
    case V(name) => facts.get(name) match {
        case None               => V(name)
        case Some(FConst(c))    => E(c, Seq())
        case Some(FCopy(other)) => V(other)
      }
    case Function(args, outty, body) =>
      Function(args, outty, propagateImpl(body, facts))
    case E(op, args) => {
      val e = E(op, args.map(propagateImpl(_, facts)))
      e match {
        case View.Negate(View.Const[Int](x))                    => View.mkConst(-x)
        case View.Plus(View.Const[Int](x), View.Const[Int](y))  => View.mkConst(x + y)
        case View.Plus(t, View.Const[Int](0))  => t
        case View.Plus(View.Const[Int](0), t)  => t
        case View.Minus(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x - y)
        case View.Minus(t, View.Const[Int](0)) => t
        case View.Minus(View.Const[Int](0), t) => View.Negate(t)
        case View.Times(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x * y)
        case View.Times(t, View.Const[Int](1)) => t
        case View.Times(View.Const[Int](1), t) => t
        case View.Times(t, View.Const[Int](0)) => View.mkConst(0)
        case View.Times(View.Const[Int](0), t) => View.mkConst(0)
        case View.Equals(View.Const[Unit](x), View.Const[Unit](y)) => View
            .mkConst(x == y)
        case View.Equals(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x == y)
        case View.Equals(View.Const[Boolean](x), View.Const[Boolean](y)) => View
            .mkConst(x == y)
        case View.Equals(View.Const[Char](x), View.Const[Char](y)) => View
            .mkConst(x == y)
        case View.Equals(View.Const[String](x), View.Const[String](y)) => View
            .mkConst(x == y)
        case View.Lt(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x < y)
        case View.Gt(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x > y)
        case View.Le(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x <= y)
        case View.Ge(View.Const[Int](x), View.Const[Int](y)) => View.mkConst(x >= y)
        case View.And(View.Const[Boolean](x), View.Const[Boolean](y)) => View
            .mkConst(x && y)
        case View.And(View.Const[Boolean](false), t) => View.mkConst(false)
        case View.And(t, View.Const[Boolean](false)) => View.mkConst(false)
        case View.And(View.Const[Boolean](true), t) => t
        case View.And(t, View.Const[Boolean](true)) => t
        case View.Or(View.Const[Boolean](x), View.Const[Boolean](y)) => View
            .mkConst(x || y)
        case View.Or(View.Const[Boolean](true), t) => View.mkConst(true)
        case View.Or(t, View.Const[Boolean](true)) => View.mkConst(true)
        case View.Or(View.Const[Boolean](false), t) => t
        case View.Or(t, View.Const[Boolean](false)) => t
        case View.IfThenElse(View.Const[Boolean](b), thent, elset) =>
          if b then thent else elset
        case _ => e
      }
    }
  }

  def classify(t: Term, facts: Map[Name, Fact]): Option[Fact] = t match {
    case V(name)                   => Some(facts.get(name).getOrElse(FCopy(name)))
    case E(c @ Op.Const(_), Seq()) => Some(FConst(c)(using c.prim))
    case _                         => None
  }
}
