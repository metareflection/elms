package lms.ir

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program

abstract class Builder {
  type Exp
  type Name

  // CR cwong: rethink this API
  def name(s: String): Name
  def fresh(): Name
  def variable(name: Name): Exp

  def fun(name: Option[String], top: Boolean, args: Seq[(Name, Type)], outty: Type)(
      body: => Exp
  ): Exp

  def lift[A: Liftable](x: A): Exp
  def reflect(op: Op, children: Seq[Exp]): Exp
  def region(f: => Exp): Exp

  def extract(): Program
}
