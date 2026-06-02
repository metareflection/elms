package elms.pipeline

import elms.core.{Type, Op, Name, StaticData}
import elms.core.tree.untyped.Program
import elms.util.Counter

abstract class Builder {
  type Exp

  protected case class FunctionStub(symbol: Exp, fill: (=> Exp) => Unit)

  private val counter = Counter()

  val staticData = scala.collection.mutable.Map[Name, StaticData]()

  def name(s: String): Name = Name.from(s)
  def fresh(): Name = Name.from(counter.tick())
  def variable(name: Name): Exp

  def fun(name: Name, top: Boolean, arg: Name, inty: Type, outty: Type): FunctionStub

  def reflect(op: Op, children: Seq[Exp]): Exp

  def region(f: => Exp): Exp

  def registerStaticData(data: StaticData): Exp = {
    val name = fresh()
    staticData(name) = data
    variable(name)
  }

  def extract(): Program
}
