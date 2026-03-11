package lms.ir.egraph

import lms.ir.Op

case class ENode(op: Op, children: Vector[Id])
