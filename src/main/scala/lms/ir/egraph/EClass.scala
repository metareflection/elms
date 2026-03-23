package lms.ir.egraph

import scala.collection.mutable

import lms.core.Op

class EClass[Data](
    val id: Id,
    val nodes: mutable.Set[ENode] = mutable.Set.empty[ENode],
    val parents: mutable.Set[EClass.Parent[Op]] = mutable.Set.empty[EClass.Parent[Op]],
    var data: Data
)

object EClass {
  case class Parent[Op](parentClass: Id, node: ENode)
}
