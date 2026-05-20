package elms.pipeline.eqsat

import scala.collection.mutable

private case class State(
    nextRun: Int = 0,
    delay: Int = 1,
    totalRuns: Int = 0,
    totalMatches: Long = 0,
)

class BackoffScheduler(
    initialDelay: Int = 1,
    maxDelay: Int = 10,
    explosiveMatchLimit: Int = 1000,
    lowYieldRatio: Double = 0.01
) extends Scheduler {
  var state = mutable.Map.empty[Expansion, State]

  def shouldRun(rule: Expansion, iteration: Int): Boolean = {
    val s = state.getOrElse(rule, State())
    iteration >= s.nextRun
  }

  def shouldBackOff(matches: Int, newNodes: Int, newUnions: Int): Boolean = {
    val usefulChanges = newNodes + newUnions
    usefulChanges == 0 ||
    (matches > explosiveMatchLimit && usefulChanges.toDouble / matches < lowYieldRatio)
  }

  def recordResult(
      rule: Expansion,
      iteration: Int,
      matches: Int,
      newNodes: Int,
      newUnions: Int
  ): Unit = {
    val old = state.getOrElse(rule, State())

    val newDelay =
      if shouldBackOff(matches, newNodes, newUnions) then {
        println(s"backing off of rule $rule")
        math.min(old.delay * 2, maxDelay)
      }
      else initialDelay

    val nextRun = iteration + newDelay

    state(rule) = old.copy(
      nextRun = nextRun,
      delay = newDelay,
      totalRuns = old.totalRuns + 1,
      totalMatches = old.totalMatches + matches,
    )
  }
}
