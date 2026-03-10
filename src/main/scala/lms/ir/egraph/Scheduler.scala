package lms.ir.egraph

trait Scheduler[Op, Data]:
  def nextBatch(all: Vector[Rule[Op, Data]], iter: Int): Vector[Rule[Op, Data]]

object Scheduler:
  def roundRobin[Op, Data]: Scheduler[Op, Data] =
    new Scheduler[Op, Data]:
      def nextBatch(all: Vector[Rule[Op, Data]], iter: Int) = all
