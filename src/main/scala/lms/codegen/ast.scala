package lms.codegen.ast

import lms.core.{Op, Type}

sealed abstract class Term

case class E(op: Op, children: Seq[Term]) extends Term
case class V(name: String) extends Term
case class Let(x: String, e1: Term, e2: Term) extends Term

case class Function(args: Seq[(String, Type)], outty: Type, body: Term) extends Term

case class Program(functions: Seq[(String, Function)])
