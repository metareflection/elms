package elms.core.tree.untyped

import elms.core.{Op, Type, Name, StaticData}
import elms.util.Plumbing.*

sealed abstract class Term derives CanEqual

case class E(op: Op, children: Seq[Term]) extends Term
case class V(name: Name) extends Term
case class Let(x: Name, e1: Term, e2: Term) extends Term

case class Function(arg: Name, inty: Type, outty: Type, body: Term) extends Term {
  def map(f: Term => Term): Function = Function(arg, inty, outty, f(body))
}

case class Program(functions: Seq[(Name, Function)], staticData: Seq[(Name, StaticData)])

object Term {
  def size(e: Term): Int = e match {
    case E(_, children) => 1 + children.map(size).sum
    case V(_) => 1
    case Let(_, e1, e2) => 1 + size(e1) + size(e2)
    case Function(_, _, _, body) => 1 + size(body)
  }
}

extension (t: Term)
  def size: Int = Term.size(t)
