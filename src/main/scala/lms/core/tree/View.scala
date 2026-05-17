package lms.core.tree

import scala.compiletime.constValue

import lms.core.{Type, Primitive, Op, Name}
import lms.runtime.Log

object View {
  private def warnTooMany(name: String): Unit = Log
    .warning(s"BUG: $name with too many children")

  private def errNone(name: String): Unit = Log.error(s"BUG: $name with no children")

  private def errTooFew(name: String): Unit = Log
    .error(s"BUG: $name with too few children")

  type Arity[N <: Int] = N match
    case 1 => Term
    case 2 => (Term, Term)
    case 3 => (Term, Term, Term)

  transparent inline def withArity[N <: Int](
      op: Op,
      name: String,
      t: Term
  ): Option[Arity[N]] = t match {
    case E(o, s) if o == op => {
      val n = constValue[N]
      if s.length == 0 then {
        errNone(name)
        None
      } else if s.length < n then {
        errTooFew(name)
        None
      } else {
        val (args, rest) = s.splitAt(n)
        if rest.nonEmpty then warnTooMany(name)
        Some(inline constValue[N] match {
          case 1 => args.head.asInstanceOf[Arity[N]]
          case 2 => (args(0), args(1)).asInstanceOf[Arity[N]]
          case 3 => (args(0), args(1), args(2)).asInstanceOf[Arity[N]]
        })
      }
    }

    case _ => None
  }

  // CR-someday cwong: It would be nice to automatically generate this and
  // Op.scala from a common place

  class Const[T: Primitive] {
    def unapply(t: Term): Option[T] = t match {
      case E(c @ Op.Const(v), s) => {
        if s.length > 0 then warnTooMany("`Const`")
        c.prim.is(summon[Primitive[T]]).map(_(v))
      }
      case _ => None
    }
  }

  def mkConst[T: Primitive](x: T): Term = E(Op.Const(x), Seq())

  object App {
    def apply(f: Term, args: Seq[Term]) = E(Op.App, f +: args)
    def unapply(t: Term): Option[(Term, Seq[Term])] = t match {
      case E(Op.App, f +: args) => Some((f, args))
      case E(Op.App, Seq())     => {
        errNone("`App`")
        None
      }
      case _ => None
    }
  }

  object Negate {
    def apply(t: Term): Term = E(Op.App, Seq(t))
    def unapply(t: Term): Option[Term] = withArity[1](Op.Negate, "`Negate`", t)
  }
  object Plus {
    def apply(t1: Term, t2: Term) = E(Op.Plus, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Plus, "`Plus`", t)
  }
  object Minus {
    def apply(t1: Term, t2: Term) = E(Op.Minus, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Minus, "Minus", t)
  }
  object Times {
    def apply(t1: Term, t2: Term) = E(Op.Times, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Times, "Times", t)
  }

  object Equals {
    def apply(t1: Term, t2: Term) = E(Op.Equals, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Equals, "Equals", t)
  }
  object Lt {
    def apply(t1: Term, t2: Term) = E(Op.Lt, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Lt, "Lt", t)
  }
  object Gt {
    def apply(t1: Term, t2: Term) = E(Op.Gt, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Gt, "Gt", t)
  }
  object Le {
    def apply(t1: Term, t2: Term) = E(Op.Le, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Le, "Le", t)
  }
  object Ge {
    def apply(t1: Term, t2: Term) = E(Op.Ge, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Ge, "Ge", t)
  }

  object And {
    def apply(t1: Term, t2: Term) = E(Op.And, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.And, "And", t)
  }
  object Or {
    def apply(t1: Term, t2: Term) = E(Op.Or, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Or, "Or", t)
  }

  object Range {
    def apply(t1: Term, t2: Term) = E(Op.Range, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] = withArity[2](Op.Range, "Range", t)
  }
  object RangeForEach {
    def unapply(t: Term): Option[(Name, Term, Term, Term)] = t match {
      case E(Op.RangeForEach(name), s) => {
        if s.length == 0 then {
          errNone("RangeForEach")
          return None
        }
        if s.length < 3 then {
          errTooFew("RangeForEach")
          return None
        }
        if s.length > 3 then { warnTooMany("RangeForEach") }
        Some((name, s(0), s(1), s(3)))
      }
      case _ => None
    }
  }
  object RangeStart {
    def apply(t: Term): Term = E(Op.RangeStart, Seq(t))
    def unapply(t: Term): Option[Term] = withArity[1](Op.RangeStart, "RangeStart", t)
  }
  object RangeEnd {
    def apply(t: Term): Term = E(Op.RangeEnd, Seq(t))
    def unapply(t: Term): Option[Term] = withArity[1](Op.RangeEnd, "RangeEnd", t)
  }

  object IfThenElse {
    def apply(cond: Term, thent: Term, elset: Term) =
      E(Op.IfThenElse, Seq(cond, thent, elset))
    def unapply(t: Term): Option[(Term, Term, Term)] =
      withArity[3](Op.IfThenElse, "IfThenElse", t)
  }

  object ArrayNew {
    def apply(ty: Type, t: Term) = E(Op.ArrayNew(ty), Seq(t))
    def unapply(t: Term): Option[(Type, Term)] = t match {
      case E(Op.ArrayNew(ty), s) => {
        if s.length == 0 then {
          errNone("ArrayNew")
          return None
        }
        if s.length > 1 then { warnTooMany("ArrayNew") }
        Some((ty, s(0)))
      }
      case _ => None
    }
  }
  object ArrayGet {
    def apply(t1: Term, t2: Term) = E(Op.ArrayGet, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term)] =
      withArity[2](Op.ArrayGet, "ArrayGet", t)
  }
  object ArraySet {
    def apply(t1: Term, t2: Term) = E(Op.ArraySet, Seq(t1, t2))
    def unapply(t: Term): Option[(Term, Term, Term)] =
      withArity[3](Op.ArraySet, "ArraySet", t)
  }
  object ArrayLength {
    def apply(t: Term): Term = E(Op.ArrayLength, Seq(t))
    def unapply(t: Term): Option[Term] = withArity[1](Op.ArrayLength, "ArrayLength", t)
  }
}
