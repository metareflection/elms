package lms.pipeline.eqsat

class NaiveScheduler extends Scheduler {
  def shouldRun(rule: Expansion, iteration: Int): Boolean = true

  def recordResult(
      exp: Expansion,
      iteration: Int,
      matches: Int,
      newNodes: Int,
      newUnions: Int
  ): Unit = ()
}
