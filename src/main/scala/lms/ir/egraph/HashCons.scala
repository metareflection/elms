package lms.ir.egraph

import scala.collection.mutable

class HashCons[Op]:

  private val table = mutable.Map.empty[ENode[Op], Id]

  def get(node: ENode[Op]): Option[Id] =
    table.get(node)

  def insert(node: ENode[Op], id: Id): Unit =
    table(node) = id

  def remove(node: ENode[Op]): Unit =
    table.remove(node)

  def contains(node: ENode[Op]): Boolean =
    table.contains(node)
