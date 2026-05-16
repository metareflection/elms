package lms.pipeline

import lms.core.{Type, Op, Name}
import lms.core.tree.Program
import lms.util.Counter

abstract class Builder {
  type Exp

  protected case class FunctionStub(
    symbol: Exp,
    fill: (=> Exp) => Unit
  )

  private val counter = Counter()

  def name(s: String): Name = Name.from(s)
  def fresh(): Name = Name.from(counter.tick())
  def variable(name: Name): Exp

  def fun(name: Name, top: Boolean, args: Seq[(Name, Type)], outty: Type):
    FunctionStub

  def reflect(op: Op, children: Seq[Exp]): Exp

  def region(f: => Exp): Exp

  def extract(): Program
}
