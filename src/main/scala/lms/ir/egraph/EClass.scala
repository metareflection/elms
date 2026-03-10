package lms.ir.egraph

import scala.collection.mutable

class EClass[Op, Data](
    val id: Id,
    val nodes: mutable.Set[ENode[Op]] = mutable.Set.empty[ENode[Op]],
    val parents: mutable.Set[EClass.Parent[Op]] = mutable.Set.empty[EClass.Parent[Op]],
    var data: Data
)

object EClass {
  case class Parent[Op](parentClass: Id, node: ENode[Op])
}
