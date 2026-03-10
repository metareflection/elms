package lms.ir.egraph

trait Analysis[Data]:
  def bottom: Data
  def join(a: Data, b: Data): Data
  def make(node: ENode, childData: Vector[Data]): Data
  def mergeClassData(a: Data, b: Data): Data = join(a, b)
