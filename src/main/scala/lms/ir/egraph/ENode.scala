package lms.ir.egraph

import lms.core.Op

case class ENode(op: Op, children: Vector[Id])
