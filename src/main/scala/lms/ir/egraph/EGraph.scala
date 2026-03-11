package lms.ir.egraph

import scala.collection.mutable

import lms.ir.Op

class EGraph[Data](analysis: Analysis[Data]):
  private val uf = new UnionFind()
  private val classes = mutable.Map.empty[Id, EClass[Data]]
  private val hashcons = new HashCons
  private val enodeToClass = mutable.Map.empty[ENode, Id]
  private val worklist = mutable.Queue.empty[Id]

  private def canon(op: Op, children: Vector[Id]): ENode =
    ENode(op, children.map(uf.find))

  private def canon(node: ENode): ENode =
    canon(node.op, node.children)

  def find(id: Id): Id =
    uf.find(id)

  def eclass(id: Id): Option[EClass[Data]] =
    classes.get(uf.find(id))

  def data(id: Id): Data =
    classes(uf.find(id)).data

  private def recomputeData(id0: Id): Unit = {
    val id = uf.find(id0)
    classes.get(id).foreach { ec =>
      var acc = analysis.bottom
      for node <- ec.nodes do
        val childData = node.children.map(ch => classes(uf.find(ch)).data)
        acc = analysis.join(acc, analysis.make(node, childData))
      ec.data = acc
    }
  }

  def add(op: Op, children: Vector[Id]): Id = {
    val node = canon(op, children)

    hashcons.get(node) match {
      case Some(existing) => uf.find(existing)

      case None => {
        val id = uf.make()
        val eclass = new EClass[Data](id, data = analysis.bottom)
        classes(id) = eclass

        eclass.nodes += node
        hashcons.insert(node, id)
        enodeToClass(node) = id

        classes(id) = eclass
        hashcons.insert(node, id)
        enodeToClass(node) = id

        for child <- node.children do
          val rc = uf.find(child)
          classes(rc).parents += EClass.Parent(id, node)
          worklist.enqueue(rc)

        recomputeData(id)

        id
      }
    }
  }

  def merge(a: Id, b: Id): Id = {
    val ra = uf.find(a)
    val rb = uf.find(b)
    if ra == rb then return ra

    val root = uf.union(ra, rb)
    val other = if root == ra then rb else ra

    val rootClass = classes(root)
    val otherClass = classes(other)

    // Union the node sets and parent edges.
    rootClass.nodes ++= otherClass.nodes
    rootClass.parents ++= otherClass.parents

    // Update analysis data cheaply; we will recompute after rebuild, but keep
    // something sane.
    rootClass.data = analysis.mergeClassData(rootClass.data, otherClass.data)

    // Update enode->class map for nodes that belonged to the removed class.
    for n <- otherClass.nodes do enodeToClass(n) = root

    classes.remove(other)

    // This merged class likely affects canonicalization of its parents.
    worklist.enqueue(root)

    root
  }

  def rebuild(): Unit = {
    // Classes whose `nodes` changed and need analysis recompute.
    val touchedForAnalysis = mutable.Set.empty[Id]

    while worklist.nonEmpty do {
      val childId0 = worklist.dequeue()
      val childId = uf.find(childId0)

      classes.get(childId).foreach { childClass =>
        // Snapshot parents because we mutate sets.
        val parentEdges = childClass.parents.toVector

        for EClass.Parent(parentClass0, parentNode0) <- parentEdges do
          val parentClassId = uf.find(parentClass0)
          classes.get(parentClassId) match
            case None =>
              // Parent class disappeared due to merges; drop stale edge.
              childClass.parents -= EClass.Parent(parentClass0, parentNode0)

            case Some(pcls) => {
              val oldNode = parentNode0
              val newNode = canon(oldNode)

              // If node changed due to child canonicalization, update:
              // - child's parent-edge set
              // - parent class's node set
              // - enodeToClass and hashcons bookkeeping
              if newNode != oldNode then {
                // Update edge in child's parent set.
                childClass.parents -= EClass.Parent(parentClass0, parentNode0)
                childClass.parents += EClass.Parent(parentClassId, newNode)

                // Update node in the parent class's node set.
                if pcls.nodes.contains(oldNode) then
                  pcls.nodes -= oldNode
                  pcls.nodes += newNode
                  touchedForAnalysis += parentClassId

                // Update enodeToClass mapping for canonical node.
                // (old mapping might still exist; we don't rely on it after this)
                enodeToClass.remove(oldNode)
                enodeToClass(newNode) = parentClassId

                // Update hashcons: old node should no longer be keyed.
                if hashcons.contains(oldNode) then hashcons.remove(oldNode)

                // Now enforce congruence via hashcons on the canonical node.
                hashcons.get(newNode) match {
                  case Some(existingClass0) =>
                    val r1 = uf.find(existingClass0)
                    val r2 = uf.find(parentClassId)
                    if r1 != r2 then
                      val merged = merge(r1, r2)
                      // Any merge potentially invalidates parent edges of
                      // merged classes.
                      worklist.enqueue(merged)
                      touchedForAnalysis += merged

                  case None =>
                    hashcons.insert(newNode, parentClassId)
                    enodeToClass(newNode) = parentClassId

                  // Even if only hashcons changed, parent class nodes are
                  // stable, analysis unchanged.
                }
              }
            }
      }
    }

    // After structural rebuild, canonicalize node sets for all classes (light,
    // but keeps invariants tight).
    //
    // Also, re-hashcons to ensure every node is indexed correctly after unions.
    // (Conservative but robust; still OK for medium workloads.)
    val allIds = classes.keys.toVector
    for id0 <- allIds do
      val id = uf.find(id0)
      classes.get(id).foreach { ec =>
        val snapshot = ec.nodes.toVector
        var changed = false
        for n <- snapshot do
          val cn = canon(n)
          if cn != n then
            ec.nodes -= n
            ec.nodes += cn
            enodeToClass.remove(n)
            enodeToClass(cn) = id
            if hashcons.contains(n) then hashcons.remove(n)
            changed = true
        if changed then touchedForAnalysis += id

        // Ensure hashcons has all canonical nodes.
        for n <- ec.nodes do
          hashcons.get(n) match
            case Some(existing) =>
              val r1 = uf.find(existing)
              val r2 = uf.find(id)
              if r1 != r2 then
                val merged = merge(r1, r2)
                touchedForAnalysis += merged
            case None =>
              hashcons.insert(n, id)
              enodeToClass(n) = id
      }

    // Finally, recompute analysis for any class that may have had node changes
    // or merges.
    for id <- touchedForAnalysis do recomputeData(id)
  }

  // Merge the classes containing these two enodes (if they exist).
  def mergeNodes(a: ENode, b: ENode): Option[Id] =
    val ca = enodeToClass.get(canon(a))
    val cb = enodeToClass.get(canon(b))
    (ca, cb) match {
      case (Some(x), Some(y)) => Some(merge(x, y))
      case _                  => None
    }

  def dump(): Unit =
    println("EGraph:")
    for (id, cls) <- classes do
      println(s"EClass $id")
      for node <- cls.nodes do
        val children = node.children.mkString(", ")
        println(s"  ${node.op}($children)")
