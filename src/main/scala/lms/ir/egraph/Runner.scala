package lms.ir.egraph

final class Runner[Op, Data](
    egraph: EGraph[Op, Data],
    rules: Vector[Rule[Op, Data]],
    cfg: Runner.Config = Runner.Config(),
    scheduler: Scheduler[Op, Data] = Scheduler.roundRobin
):
  case class Result(iterations: Int, totalApplications: Int, saturated: Boolean)

  def run(): Result =
    var iter = 0
    var total = 0
    var saturated = false

    while iter < cfg.maxIterations && !saturated do
      var changed = false
      iter += 1

      val batch = scheduler.nextBatch(rules, iter)
      for r <- batch do
        val d = r.run(egraph)
        if (d) {
          changed = true
          total += 1
        }

      egraph.rebuild()

      if !changed then saturated = true

    Result(iter, total, saturated)

object Runner {
  case class Config(
      maxIterations: Int = 20,
      rebuildEvery: Int = 1,
      nodeLimit: Int = 200_000,
      timeLimitMs: Long = 0L
  )
}
