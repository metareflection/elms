package lms.pipeline.eqsat

import lms.core.Op

enum Pattern {
  case Var(name: String)
  case Node(op: Op.Pure, children: Vector[Pattern])
}

case class Expansion(lhs: Pattern, rhs: Pattern)

abstract class Rule private (
    val lhs: Pattern,
    val rhs: Pattern,
    val symmetric: Boolean
) {
  def expansions: Seq[Expansion] =
    if symmetric then Seq(Expansion(lhs, rhs), Expansion(rhs, lhs))
    else Seq(Expansion(lhs, rhs))
}

object Rule {
  private case class Rewrite(lhsp: Pattern, rhsp: Pattern) extends Rule(lhsp, rhsp, false)
  private case class Equivalence(lhsp: Pattern, rhsp: Pattern)
      extends Rule(lhsp, rhsp, true)

  def rewrite(lhs: Pattern, rhs: Pattern): Rule = Rewrite(lhs, rhs)
  def equivalence(lhs: Pattern, rhs: Pattern): Rule = Equivalence(lhs, rhs)
}

class Ruleset(ruleDecls: Seq[Rule]) {
  val rules: Set[Expansion] = ruleDecls.flatMap(_.expansions).toSet
  val toSeq = rules.toSeq
}
