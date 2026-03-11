package lms.core

import lms.ir.Op

abstract class Dialect {
  type Exp

  def lift[A:DslType](x: A): Exp
  def reflect(op: Op, children: Vector[Exp]): Exp
  def reify(root: Boolean, f: Exp => Exp): Exp
}
