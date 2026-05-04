package lms.ir.opt

import lms.core.Op

import EGraph.EClass

enum Stmt {
  case Return(node: EClass)
  case Let(x: String, lhs: Stmt, tail: Stmt)
  case Effect(op: Op.Effectful, children: Seq[EClass])
  case If(cond: EClass, thn: Stmt, els: Stmt)
  case RangeFor(v: String, st: EClass, end: EClass, body: Stmt)
}
