package lms.core.tree

import lms.core.{Op, Type, Name}
import lms.util.Plumbing.*

sealed abstract class Term derives CanEqual

case class E(op: Op, children: Seq[Term]) extends Term
case class V(name: Name) extends Term
case class Let(x: Name, e1: Term, e2: Term) extends Term

case class Function(args: Seq[(Name, Type)], outty: Type, body: Term) extends Term

case class Program(functions: Seq[(Name, Function)])

object Term {
  def size(e: Term): Int = e match {
    case E(_, children) => 1 + children.map(size).sum
    case V(_) => 1
    case Let(_, e1, e2) => 1 + size(e1) + size(e2)
    case Function(_, _, body) => 1 + size(body)
  }
}

extension (t: Term)
  def size: Int = Term.size(t)
