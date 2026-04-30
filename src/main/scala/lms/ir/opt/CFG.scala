package lms.ir.opt

import lms.core.Op

import EGraph.EClass

enum Stmt {
  case Return(node: EClass)
  case Block(body: Seq[Stmt])
  case Effect(op: Op.Effectful, children: Seq[EClass])
  case IfThenElse(cond: EClass, thn: Block, els: Block)
}
