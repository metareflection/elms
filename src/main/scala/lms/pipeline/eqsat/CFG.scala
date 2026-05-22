package elms.pipeline.eqsat

import elms.core.{Op, Type, Name}

import EGraph.EClass

enum Stmt {
  case Return(node: EClass)
  case Let(x: Name, lhs: Stmt, tail: Stmt)
  case Effect(op: Op.Effectful, children: Seq[EClass])
  case If(cond: EClass, thn: Stmt, els: Stmt)
  case RangeFor(v: Name, st: EClass, end: EClass, body: Stmt)
  case While(cond: Stmt, body: Stmt)
  case Lambda(args: Seq[(Name, Type)], outty: Type, body: Stmt)
}
