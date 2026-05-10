package lms.ir

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program
import lms.util.Counter

abstract class Builder {
  type Exp

  protected case class FunctionStub(
    symbol: Exp,
    fill: (=> Exp) => Unit
  )

  private val counter = Counter()

  def name(s: String): Name = Named(s)
  def fresh(): Name = Fresh(counter.tick())
  def variable(name: Name): Exp

  def fun(name: Name, top: Boolean, args: Seq[(Name, Type)], outty: Type):
    FunctionStub

  def lift[A: Liftable](x: A): Exp
  def reflect(op: Op, children: Seq[Exp]): Exp

  def region(f: => Exp): Exp

  def extract(): Program
}
