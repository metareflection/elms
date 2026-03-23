package lms.codegen

import lms.core.{Op, Type}

package object ast {
  sealed abstract class Term

  case class E(op: Op, children: Seq[Term]) extends Term
  case class V(name: String) extends Term

  case class Function(
      args: Seq[(String, Type)],
      outty: Type,
      body: Seq[Term]
  )

  case class Program(functions: Seq[(String, Function)])
}
