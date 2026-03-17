package lms.core

import lms.ir.Op

abstract class Dialect {
  type Exp
  type Name

  def fresh(): Name
  def v(name: Name): Exp

  def root(args: Seq[Name])(body: => Exp): Exp

  def lift[A:DslType](x: A): Exp
  def reflect(op: Op, children: Seq[Exp]): Exp
  def region(f: => Exp): Exp
}
