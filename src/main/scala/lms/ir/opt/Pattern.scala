package lms.ir.opt

import lms.core.Op

enum Pattern {
  case Var(name: String)
  case Node(op: Op.Pure, children: Vector[Pattern])
}

sealed trait Rule {
  val lhs: Pattern
  val rhs: Pattern
  val oneWay: Boolean
}

object Rule {
  def unapply(rule: Rule): Option[(Pattern, Pattern, Boolean)] =
    Some((rule.lhs, rule.rhs, rule.oneWay))
}

case class Rewrite(lhs: Pattern, rhs: Pattern) extends Rule {
  override val oneWay = true
}

case class Equivalence(lhs: Pattern, rhs: Pattern) extends Rule {
  override val oneWay = false
}
