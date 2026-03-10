package lms.ir.egraph

trait Scheduler[Data]:
  def nextBatch(all: Vector[Rule[Data]], iter: Int): Vector[Rule[Data]]

object Scheduler:
  def roundRobin[Op, Data]: Scheduler[Data] =
    new Scheduler[Data]:
      def nextBatch(all: Vector[Rule[Data]], iter: Int) = all
