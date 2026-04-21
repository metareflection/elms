package lms.ir.opt

import lms.core.Op

enum Stmt {
  case Effect(dst: String, op: Op, children: Seq[String])
  case Block(body: Seq[Stmt])
  case IfThenElse(cond: String, thn: Block, els: Block)
  // TODO: Think about how to represent whiles
}
