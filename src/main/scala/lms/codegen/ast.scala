package lms.codegen.ast

import lms.core.{Op, Type}

sealed abstract class Term

case class E(op: Op, children: Seq[Term]) extends Term
case class V(name: String) extends Term
case class Let(x: String, e1: Term, e2: Term) extends Term

case class Function(args: Seq[(String, Type)], outty: Type, body: Term) extends Term

case class Program(functions: Seq[(String, Function)])

object Term {
  def size(e: Term): Int = e match {
    case E(_, children) => 1 + children.map(size).sum
    case V(_) => 1
    case Let(_, e1, e2) => 1 + size(e1) + size(e2)
    case Function(_, _, body) => 1 + size(body)
  }
}
