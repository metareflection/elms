package lms.ir

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program
import lms.util.Counter

trait Builder {
  type Exp

  private val counter = Counter()

  def name(s: String): Name = Named(s)
  def fresh(): Name = Fresh(counter.tick())
  def variable(name: Name): Exp

  def fun(name: Option[Name], top: Boolean, args: Seq[(Name, Type)], outty: Type)(
      body: => Exp
  ): Exp

  def lift[A: Liftable](x: A): Exp
  def reflect(op: Op, children: Seq[Exp]): Exp

  def region(f: => Exp): Exp

  def extract(): Program
}
