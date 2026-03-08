package lms.backends.egraph

case class ENode[Op](op: Op, children: Vector[Id])
