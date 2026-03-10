package lms.ir.egraph

case class ENode[Op](op: Op, children: Vector[Id])
