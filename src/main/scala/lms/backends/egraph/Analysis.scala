package lms.backends.egraph

trait Analysis[Op, Data]:
  def bottom: Data
  def join(a: Data, b: Data): Data
  def make(node: ENode[Op], childData: Vector[Data]): Data
  def mergeClassData(a: Data, b: Data): Data = join(a, b)
