package lms.ir.eqsat

import lms.core.Op
import lms.ir.Name

import EGraph.EClass

enum Stmt {
  case Return(node: EClass)
  case Let(x: Name, lhs: Stmt, tail: Stmt)
  case Effect(op: Op.Effectful, children: Seq[EClass])
  case If(cond: EClass, thn: Stmt, els: Stmt)
  case RangeFor(v: String, st: EClass, end: EClass, body: Stmt)
}
