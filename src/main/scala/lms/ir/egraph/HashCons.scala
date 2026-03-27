package lms.ir.egraph

import scala.collection.mutable

class HashCons[Op]:

  private val table = mutable.Map.empty[ENode, Id]

  def get(node: ENode): Option[Id] = table.get(node)

  def insert(node: ENode, id: Id): Unit = table(node) = id

  def remove(node: ENode): Unit = table.remove(node)

  def contains(node: ENode): Boolean = table.contains(node)
