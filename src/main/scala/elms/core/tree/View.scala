package elms.core.tree

import elms.core.{Type, Primitive, Op, Name, StructRepr}
import elms.runtime.Log

def view(t: Term): Option[View] = View.view(t)

sealed trait View {
  def into: Term
}

object View {
  private def warnTooMany(name: String): Unit = Log
    .warning(s"BUG: $name with too many children")

  private def errNone(name: String): Unit = Log.error(s"BUG: $name with no children")

  private def errTooFew(name: String): Unit = Log
    .error(s"BUG: $name with too few children")

  private def children(name: String, s: Seq[Term], n: Int): Option[Seq[Term]] =
    if s.isEmpty then {
      errNone(name)
      None
    } else if s.length < n then {
      errTooFew(name)
      None
    } else {
      val (args, rest) = s.splitAt(n)
      if rest.nonEmpty then warnTooMany(name)
      Some(args)
    }

  private def arity1(name: String, s: Seq[Term]): Option[Term] = children(name, s, 1)
    .map(_(0))

  private def arity2(name: String, s: Seq[Term]): Option[(Term, Term)] =
    children(name, s, 2).map(args => (args(0), args(1)))

  private def arity3(name: String, s: Seq[Term]): Option[(Term, Term, Term)] =
    children(name, s, 3).map(args => (args(0), args(1), args(2)))

  final case class V(name: Name) extends View {
    def into: Term = elms.core.tree.V(name)
  }

  final case class Function(arg: Name, inty: Type, outty: Type, body: Term)
      extends View {
    def into: Term = elms.core.tree.Function(arg, inty, outty, body)
  }

  final case class Const[T](value: T)(using val prim: Primitive[T]) extends View {
    def into: Term = E(Op.Const(value), Seq())
  }

  final case class Let(x: Name, mty: Option[Type], e1: Term, e2: Term) extends View {
    def into: Term = {
      val bound = mty match {
        case Some(ty) => VarNew(ty, e1).into
        case None     => e1
      }
      elms.core.tree.Let(x, bound, e2)
    }
  }

  final case class Custom(name: String, ty: Type, args: Seq[Term]) extends View {
    def into: Term = E(Op.Custom(name, ty), args)
  }
  final case class App(f: Term, args: Seq[Term]) extends View {
    def into: Term = E(Op.App, f +: args)
  }

  final case class VarNew(ty: Type, t: Term) extends View {
    def into: Term = E(Op.VarNew(ty), Seq(t))
  }

  final case class VarGet(t: Term) extends View {
    def into: Term = E(Op.VarGet, Seq(t))
  }

  final case class VarSet(t: Term, v: Term) extends View {
    def into: Term = E(Op.VarSet, Seq(t, v))
  }

  final case class Negate(t: Term) extends View {
    def into: Term = E(Op.Negate, Seq(t))
  }

  final case class Plus(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Plus, Seq(t1, t2))
  }

  final case class Minus(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Minus, Seq(t1, t2))
  }

  final case class Times(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Times, Seq(t1, t2))
  }

  final case class Equals(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Equals, Seq(t1, t2))
  }

  final case class Lt(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Lt, Seq(t1, t2))
  }

  final case class Gt(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Gt, Seq(t1, t2))
  }

  final case class Le(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Le, Seq(t1, t2))
  }

  final case class Ge(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Ge, Seq(t1, t2))
  }

  final case class And(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.And, Seq(t1, t2))
  }

  final case class Or(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Or, Seq(t1, t2))
  }

  final case class Not(t: Term) extends View {
    def into: Term = E(Op.Not, Seq(t))
  }

  final case class Range(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.Range, Seq(t1, t2))
  }

  final case class RangeForEach(name: Name, start: Term, end: Term, body: Term)
      extends View {
    def into: Term = E(Op.RangeForEach(name), Seq(start, end, body))
  }

  final case class RangeStart(t: Term) extends View {
    def into: Term = E(Op.RangeStart, Seq(t))
  }

  final case class RangeEnd(t: Term) extends View {
    def into: Term = E(Op.RangeEnd, Seq(t))
  }

  final case class IfThenElse(cond: Term, thent: Term, elset: Term) extends View {
    def into: Term = E(Op.IfThenElse, Seq(cond, thent, elset))
  }

  final case class While(guard: Term, body: Term) extends View {
    def into: Term = E(Op.While, Seq(guard, body))
  }

  final case class ArrayNew(ty: Type, t: Term) extends View {
    def into: Term = E(Op.ArrayNew(ty), Seq(t))
  }

  final case class ArrayGet(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.ArrayGet, Seq(t1, t2))
  }

  final case class ArraySet(t1: Term, t2: Term, t3: Term) extends View {
    def into: Term = E(Op.ArraySet, Seq(t1, t2, t3))
  }

  final case class ArrayLength(t: Term) extends View {
    def into: Term = E(Op.ArrayLength, Seq(t))
  }

  final case class StringLength(t: Term) extends View {
    def into: Term = E(Op.StringLength, Seq(t))
  }

  final case class StringTake(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.StringTake, Seq(t1, t2))
  }

  final case class StringDrop(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.StringDrop, Seq(t1, t2))
  }

  final case class StringStartsWith(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.StringStartsWith, Seq(t1, t2))
  }

  final case class StringCharAt(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.StringCharAt, Seq(t1, t2))
  }

  final case class StringEndsWith(t1: Term, t2: Term) extends View {
    def into: Term = E(Op.StringEndsWith, Seq(t1, t2))
  }

  final case class StringSubstring(t1: Term, t2: Term, t3: Term) extends View {
    def into: Term = E(Op.StringSubstring, Seq(t1, t2, t3))
  }

  final case class StructGet(repr: StructRepr, t: Term, field: String) extends View {
    def into: Term = E(Op.StructGet(repr, field), Seq(t))
  }

  final case class StructSet(t: Term, field: String, v: Term) extends View {
    def into: Term = E(Op.StructSet(field), Seq(t, v))
  }


  private def packConst[A](c: Op.Const[A])(using aprim: Primitive[A]): View =
    Const(c.v)(using aprim)

  def mkConst[T: Primitive](x: T): Term = Const(x).into

  def view(t: Term): Option[View] = t match {
    case elms.core.tree.V(name) => Some(V(name))

    case elms.core.tree.Function(arg, inty, outty, body) =>
      Some(Function(arg, inty, outty, body))

    case elms.core.tree.Let(x, me1, e2) =>
      val (mty, e1) = me1 match {
        case E(Op.VarNew(ty), s) => arity1("VarNew", s).map(e => (Some(ty), e))
            .getOrElse((None, me1))
        case _ => (None, me1)
      }
      Some(Let(x, mty, e1, e2))

    case E(c @ Op.Const(_), s) => {
      if s.nonEmpty then warnTooMany("`Const`")
      Some(packConst(c)(using c.prim))
    }

    case E(Op.Custom(name, ty), args) => Some(Custom(name, ty, args))

    case E(Op.App, s) => s.toList match {
        case f :: args => Some(App(f, args))
        case Nil       => {
          errNone("`App`")
          None
        }
      }

    case E(Op.VarNew(ty), s) => arity1("VarNew", s).map(VarNew(ty, _))
    case E(Op.VarGet, s)     => arity1("`VarGet`", s).map(VarGet(_))
    case E(Op.VarSet, s)     => arity2("`VarSet`", s).map(VarSet(_, _))

    case E(Op.Negate, s) => arity1("`Negate`", s).map(Negate(_))
    case E(Op.Plus, s)   => arity2("`Plus`", s).map(Plus(_, _))
    case E(Op.Minus, s)  => arity2("Minus", s).map(Minus(_, _))
    case E(Op.Times, s)  => arity2("Times", s).map(Times(_, _))

    case E(Op.Equals, s) => arity2("Equals", s).map(Equals(_, _))
    case E(Op.Lt, s)     => arity2("Lt", s).map(Lt(_, _))
    case E(Op.Gt, s)     => arity2("Gt", s).map(Gt(_, _))
    case E(Op.Le, s)     => arity2("Le", s).map(Le(_, _))
    case E(Op.Ge, s)     => arity2("Ge", s).map(Ge(_, _))

    case E(Op.And, s) => arity2("And", s).map(And(_, _))
    case E(Op.Or, s)  => arity2("Or", s).map(Or(_, _))
    case E(Op.Not, s) => arity1("Not", s).map(Not(_))

    case E(Op.Range, s)              => arity2("Range", s).map(Range(_, _))
    case E(Op.RangeForEach(name), s) => arity3("RangeForEach", s)
        .map(RangeForEach(name, _, _, _))
    case E(Op.RangeStart, s) => arity1("RangeStart", s).map(RangeStart(_))
    case E(Op.RangeEnd, s)   => arity1("RangeEnd", s).map(RangeEnd(_))

    case E(Op.IfThenElse, s) => arity3("IfThenElse", s).map(IfThenElse(_, _, _))
    case E(Op.While, s)      => arity2("While", s).map(While(_, _))

    case E(Op.ArrayNew(ty), s) => arity1("ArrayNew", s).map(ArrayNew(ty, _))
    case E(Op.ArrayInit(_), s) => ???
    case E(Op.ArrayGet, s)     => arity2("ArrayGet", s).map(ArrayGet(_, _))
    case E(Op.ArraySet, s)     => arity3("ArraySet", s).map(ArraySet(_, _, _))
    case E(Op.ArrayLength, s)  => arity1("ArrayLength", s).map(ArrayLength(_))

    case E(Op.StringLength, s)     => arity1("StringLength", s).map(StringLength(_))
    case E(Op.StringTake, s)       => arity2("StringTake", s).map(StringTake(_, _))
    case E(Op.StringDrop, s)       => arity2("StringDrop", s).map(StringDrop(_, _))
    case E(Op.StringStartsWith, s) => arity2("StringStartsWith", s)
        .map(StringStartsWith(_, _))
    case E(Op.StringCharAt, s)   => arity2("StringCharAt", s).map(StringCharAt(_, _))
    case E(Op.StringEndsWith, s) => arity2("StringEndsWith", s)
        .map(StringEndsWith(_, _))
    case E(Op.StringSubstring, s) => arity3("StringSubstring", s)
        .map(StringSubstring(_, _, _))

    case E(Op.StructGet(repr, field), s) => arity1("StructGet", s)
        .map(StructGet(repr, _, field))
    case E(Op.StructSet(field), s) => arity2("StructSet", s).map(StructSet(_, field, _))
  }

  object Extractors {
    class Const[T: Primitive] {
      def unapply(t: Term): Option[T] = View.view(t).flatMap {
        case c @ View.Const(_) => c.prim.is(summon[Primitive[T]]).map(_(c.value))
        case _                 => None
      }
    }

    object Let {
      def unapply(t: Term): Option[(Name, Option[Type], Term, Term)] = View.view(t)
        .collect { case View.Let(x, mty, e1, e2) => (x, mty, e1, e2) }
    }

    def mkConst[T: Primitive](x: T): Term = View.mkConst(x)

    object Custom {
      def apply(name: String, ty: Type, args: Seq[Term]): Term = View
        .Custom(name, ty, args).into
      def unapply(t: Term): Option[(String, Type, Seq[Term])] = View.view(t)
        .collect { case View.Custom(name, ty, args) => (name, ty, args) }
    }
    object App {
      def apply(f: Term, args: Seq[Term]): Term = View.App(f, args).into
      def unapply(t: Term): Option[(Term, Seq[Term])] = View.view(t)
        .collect { case View.App(f, args) => (f, args) }
    }

    object VarNew {
      def apply(ty: Type, t: Term): Term = View.VarNew(ty, t).into
      def unapply(t: Term): Option[(Type, Term)] = View.view(t)
        .collect { case View.VarNew(ty, e) => (ty, e) }
    }

    object VarGet {
      def apply(t: Term): Term = View.VarGet(t).into
      def unapply(t: Term): Option[Term] = View.view(t).collect { case View.VarGet(e) =>
        e
      }
    }

    object VarSet {
      def apply(t: Term, v: Term): Term = View.VarSet(t, v).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.VarSet(t, v) => (t, v) }
    }

    object Negate {
      def apply(t: Term): Term = View.Negate(t).into
      def unapply(t: Term): Option[Term] = View.view(t).collect { case View.Negate(e) =>
        e
      }
    }

    object Plus {
      def apply(t1: Term, t2: Term): Term = View.Plus(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Plus(t1, t2) => (t1, t2) }
    }

    object Minus {
      def apply(t1: Term, t2: Term): Term = View.Minus(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Minus(t1, t2) => (t1, t2) }
    }

    object Times {
      def apply(t1: Term, t2: Term): Term = View.Times(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Times(t1, t2) => (t1, t2) }
    }

    object Equals {
      def apply(t1: Term, t2: Term): Term = View.Equals(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Equals(t1, t2) => (t1, t2) }
    }

    object Lt {
      def apply(t1: Term, t2: Term): Term = View.Lt(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Lt(t1, t2) => (t1, t2) }
    }

    object Gt {
      def apply(t1: Term, t2: Term): Term = View.Gt(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Gt(t1, t2) => (t1, t2) }
    }

    object Le {
      def apply(t1: Term, t2: Term): Term = View.Le(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Le(t1, t2) => (t1, t2) }
    }

    object Ge {
      def apply(t1: Term, t2: Term): Term = View.Ge(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Ge(t1, t2) => (t1, t2) }
    }

    object And {
      def apply(t1: Term, t2: Term): Term = View.And(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.And(t1, t2) => (t1, t2) }
    }

    object Or {
      def apply(t1: Term, t2: Term): Term = View.Or(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Or(t1, t2) => (t1, t2) }
    }

    object Not {
      def apply(t: Term): Term = View.Not(t).into
      def unapply(t: Term): Option[Term] = View.view(t).collect { case View.Not(e) =>
        e
      }
    }

    object Range {
      def apply(t1: Term, t2: Term): Term = View.Range(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.Range(t1, t2) => (t1, t2) }
    }

    object RangeForEach {
      def apply(name: Name, start: Term, end: Term, body: Term): Term = View
        .RangeForEach(name, start, end, body).into
      def unapply(t: Term): Option[(Name, Term, Term, Term)] = View.view(t)
        .collect { case View.RangeForEach(name, start, end, body) =>
          (name, start, end, body)
        }
    }

    object RangeStart {
      def apply(t: Term): Term = View.RangeStart(t).into
      def unapply(t: Term): Option[Term] = View.view(t)
        .collect { case View.RangeStart(e) => e }
    }

    object RangeEnd {
      def apply(t: Term): Term = View.RangeEnd(t).into
      def unapply(t: Term): Option[Term] = View.view(t)
        .collect { case View.RangeEnd(e) => e }
    }

    object IfThenElse {
      def apply(cond: Term, thent: Term, elset: Term): Term = View
        .IfThenElse(cond, thent, elset).into
      def unapply(t: Term): Option[(Term, Term, Term)] = View.view(t)
        .collect { case View.IfThenElse(cond, thent, elset) => (cond, thent, elset) }
    }

    object While {
      def apply(guard: Term, body: Term): Term = View.While(guard, body).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.While(guard, body) => (guard, body) }
    }

    object ArrayNew {
      def apply(ty: Type, t: Term): Term = View.ArrayNew(ty, t).into
      def unapply(t: Term): Option[(Type, Term)] = View.view(t)
        .collect { case View.ArrayNew(ty, e) => (ty, e) }
    }

    object ArrayGet {
      def apply(t1: Term, t2: Term): Term = View.ArrayGet(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.ArrayGet(t1, t2) => (t1, t2) }
    }

    object ArraySet {
      def apply(t1: Term, t2: Term, t3: Term): Term = View.ArraySet(t1, t2, t3).into
      def unapply(t: Term): Option[(Term, Term, Term)] = View.view(t)
        .collect { case View.ArraySet(t1, t2, t3) => (t1, t2, t3) }
    }

    object ArrayLength {
      def apply(t: Term): Term = View.ArrayLength(t).into
      def unapply(t: Term): Option[Term] = View.view(t)
        .collect { case View.ArrayLength(e) => e }
    }

    object StringLength {
      def apply(t: Term): Term = View.StringLength(t).into
      def unapply(t: Term): Option[Term] = View.view(t)
        .collect { case View.StringLength(e) => e }
    }

    object StringTake {
      def apply(t1: Term, t2: Term): Term = View.StringTake(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.StringTake(t1, t2) => (t1, t2) }
    }

    object StringDrop {
      def apply(t1: Term, t2: Term): Term = View.StringDrop(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.StringDrop(t1, t2) => (t1, t2) }
    }

    object StringStartsWith {
      def apply(t1: Term, t2: Term): Term = View.StringStartsWith(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.StringStartsWith(t1, t2) => (t1, t2) }
    }

    object StringCharAt {
      def apply(t1: Term, t2: Term): Term = View.StringCharAt(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.StringCharAt(t1, t2) => (t1, t2) }
    }

    object StringEndsWith {
      def apply(t1: Term, t2: Term): Term = View.StringEndsWith(t1, t2).into
      def unapply(t: Term): Option[(Term, Term)] = View.view(t)
        .collect { case View.StringEndsWith(t1, t2) => (t1, t2) }
    }

    object StringSubstring {
      def apply(t1: Term, t2: Term, t3: Term): Term = View.StringSubstring(t1, t2, t3)
        .into
      def unapply(t: Term): Option[(Term, Term, Term)] = View.view(t)
        .collect { case View.StringSubstring(t1, t2, t3) => (t1, t2, t3) }
    }

    object StructGet {
      def apply(repr: StructRepr, t: Term, field: String): Term = View
        .StructGet(repr, t, field).into
      def unapply(t: Term): Option[(StructRepr, Term, String)] = View.view(t)
        .collect { case View.StructGet(repr, e, field) => (repr, e, field) }
    }

    object StructSet {
      def apply(t: Term, field: String, v: Term): Term = View.StructSet(t, field, v)
        .into
      def unapply(t: Term): Option[(Term, String, Term)] = View.view(t)
        .collect { case View.StructSet(t, field, v) => (t, field, v) }
    }
  }
}
