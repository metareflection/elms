package lms.ir

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program

abstract class Dialect {
  type Exp
  type Name

  // CR cwong: Instead of providing `init`, we should make `Dialect` a class
  // that can be instantiated separately by the user.
  def init(): Unit

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
