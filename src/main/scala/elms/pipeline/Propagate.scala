package elms.pipeline

import elms.core.{Name, Primitive}
import elms.core.given
import elms.core.Op
import elms.core.tree.*
import elms.core.tree.View.Extractors as Ext

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
    case Function(arg, inty, outty, body) =>
      Function(arg, inty, outty, propagateImpl(body, facts))
    case E(op, args) => {
      val e = E(op, args.map(propagateImpl(_, facts)))
      e match {
        case Ext.Negate(Ext.Const[Int](x))                    => Ext.mkConst(-x)
        case Ext.Plus(Ext.Const[Int](x), Ext.Const[Int](y))  => Ext.mkConst(x + y)
        case Ext.Plus(t, Ext.Const[Int](0))                   => t
        case Ext.Plus(Ext.Const[Int](0), t)                   => t
        case Ext.Minus(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x - y)
        case Ext.Minus(t, Ext.Const[Int](0))                  => t
        case Ext.Minus(Ext.Const[Int](0), t)                  => Ext.Negate(t)
        case Ext.Times(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x * y)
        case Ext.Times(t, Ext.Const[Int](1))                  => t
        case Ext.Times(Ext.Const[Int](1), t)                  => t
        case Ext.Times(t, Ext.Const[Int](0))                  => Ext.mkConst(0)
        case Ext.Times(Ext.Const[Int](0), t)                  => Ext.mkConst(0)
        case Ext.Equals(Ext.Const[Unit](x), Ext.Const[Unit](y)) => Ext
            .mkConst(x == y)
        case Ext.Equals(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x == y)
        case Ext.Equals(Ext.Const[Boolean](x), Ext.Const[Boolean](y)) => Ext
            .mkConst(x == y)
        case Ext.Equals(Ext.Const[Char](x), Ext.Const[Char](y)) => Ext
            .mkConst(x == y)
        case Ext.Equals(Ext.Const[String](x), Ext.Const[String](y)) => Ext
            .mkConst(x == y)
        case Ext.Lt(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x < y)
        case Ext.Gt(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x > y)
        case Ext.Le(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x <= y)
        case Ext.Ge(Ext.Const[Int](x), Ext.Const[Int](y)) => Ext.mkConst(x >= y)
        case Ext.And(Ext.Const[Boolean](x), Ext.Const[Boolean](y)) => Ext
            .mkConst(x && y)
        case Ext.And(Ext.Const[Boolean](false), t) => Ext.mkConst(false)
        case Ext.And(t, Ext.Const[Boolean](false)) => Ext.mkConst(false)
        case Ext.And(Ext.Const[Boolean](true), t)  => t
        case Ext.And(t, Ext.Const[Boolean](true))  => t
        case Ext.Or(Ext.Const[Boolean](x), Ext.Const[Boolean](y)) => Ext
            .mkConst(x || y)
        case Ext.Or(Ext.Const[Boolean](true), t)                 => Ext.mkConst(true)
        case Ext.Or(t, Ext.Const[Boolean](true))                 => Ext.mkConst(true)
        case Ext.Or(Ext.Const[Boolean](false), t)                => t
        case Ext.Or(t, Ext.Const[Boolean](false))                => t
        case Ext.Not(Ext.Const[Boolean](b))                      => Ext.mkConst(!b)
        case Ext.IfThenElse(Ext.Const[Boolean](b), thent, elset) =>
          if b then thent else elset
        case Ext.StringLength(Ext.Const[String](s)) => Ext.mkConst(s.length)
        case Ext.StringCharAt(Ext.Const[String](s), Ext.Const[Int](i)) => s.lift(i)
            .map(Ext.mkConst).getOrElse(e)
        case Ext.StringTake(Ext.Const[String](s), Ext.Const[Int](i)) => Ext
            .mkConst(s.take(i))
        case Ext.StringDrop(Ext.Const[String](s), Ext.Const[Int](i)) => Ext
            .mkConst(s.drop(i))
        case Ext
              .StringStartsWith(Ext.Const[String](s), Ext.Const[String](haystack)) =>
          Ext.mkConst(s.startsWith(haystack))
        case Ext.StringEndsWith(Ext.Const[String](s), Ext.Const[String](haystack)) =>
          Ext.mkConst(s.endsWith(haystack))
        case Ext.StringSubstring(
              Ext.Const[String](s),
              Ext.Const[Int](st),
              Ext.Const[Int](end)
            ) => Ext.mkConst(s.slice(st, end))
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
