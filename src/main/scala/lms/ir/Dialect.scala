package lms.ir

import lms.core.Liftable

abstract class Dialect {
  type Exp
  type Name

  def init(): Unit

  def fresh(): Name
  def variable(name: Name): Exp

  def lam(top: Boolean, args: Seq[(Name, Type)], outty: Type)(body: => Exp): Exp

  def lift[A:Liftable](x: A): Exp
  def reflect(op: Op, children: Seq[Exp]): Exp
  def region(f: => Exp): Exp
}
