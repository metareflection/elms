package lms.ir

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program
import lms.util.typeclasses.*

trait Builder {
  type Exp

  // CR cwong: Rethink this API. It is natural to assume that some backends may
  // want different `Name` types, but in practice this is beginning to feel
  // overengineered and not very good.
  type Name: Nameable
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
