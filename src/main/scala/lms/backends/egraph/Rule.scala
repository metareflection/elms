package lms.backends.egraph

trait Rule[Op, Data]:
  def name: String
  def run(egraph: EGraph[Op, Data]): Boolean
