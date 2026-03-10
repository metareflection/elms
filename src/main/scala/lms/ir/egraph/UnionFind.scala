package lms.ir.egraph

class UnionFind(initial: Int = 16):

  private var parent: Array[Int] = Array.tabulate(initial)(identity)
  private var rank: Array[Int] = Array.fill(initial)(0)
  private var next: Int = initial

  private def grow(): Unit =
    val newSize = parent.length * 2
    parent = parent ++ Array.tabulate(newSize - parent.length)(i => parent.length + i)
    rank = rank ++ Array.fill(newSize - rank.length)(0)

  def make(): Id =
    if next >= parent.length then grow()
    val id = next
    parent(id) = id
    rank(id) = 0
    next += 1
    id

  def find(x: Id): Id =
    if parent(x) != x then parent(x) = find(parent(x))
    parent(x)

  def union(a: Id, b: Id): Id =
    val ra = find(a)
    val rb = find(b)

    if ra == rb then return ra

    if rank(ra) < rank(rb) then
      parent(ra) = rb
      rb
    else if rank(ra) > rank(rb) then
      parent(rb) = ra
      ra
    else
      parent(rb) = ra
      rank(ra) += 1
      ra
