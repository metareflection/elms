package lms.ir.opt

import lms.core.Op

import EGraph.EClass

enum Stmt {
  case Effect(dst: String, op: Op, children: Seq[EClass])
  case Block(body: Seq[Stmt])
  case IfThenElse(cond: EClass, thn: Block, els: Block)
  // TODO: Think about how to represent whiles
}
