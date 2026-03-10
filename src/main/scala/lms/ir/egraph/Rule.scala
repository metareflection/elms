package lms.ir.egraph

trait Rule[Data]:
  def name: String
  def run(egraph: EGraph[Data]): Boolean
