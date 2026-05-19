package lms.pipeline.eqsat

trait Scheduler {
  def shouldRun(rule: Expansion, iteration: Int): Boolean

  def recordResult(
      rule: Expansion,
      iteration: Int,
      matches: Int,
      newNodes: Int,
      newUnions: Int
  ): Unit
}

