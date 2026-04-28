package lms.ir.opt

import scala.collection.mutable

import lms.core.{Liftable, Type, Op}
import lms.codegen.ast.Program
import lms.ir
import lms.util.Counter

class Builder extends ir.Builder {
  type Exp = EGraph.EClass
  sealed trait Name

  object Name {
    val rawNames: mutable.Set[String] = mutable.Set.empty
  }

  private case class Raw(s: String) extends Name
  private case class Fresh(i: Int) extends Name

  // TODO: This should really be tracked by the EGraph.
  var counter = Counter()

  def name(s: String): Name = Raw(s)
  def fresh(): Name = Fresh(counter.tick())

  def variable(name: Name): Exp = ???

  def fun(name: Option[String], top: Boolean, args: Seq[(Name, Type)], outty: Type)(
      body: => Exp
  ): Exp = ???

  def lift[A: Liftable](x: A): Exp = ???
  def reflect(op: Op, children: Seq[Exp]): Exp = ???
  def region(f: => Exp): Exp = ???

  def extract(): Program = ???
}
