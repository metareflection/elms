package lms.pipeline.eqsat

import scala.collection.mutable
import scala.language.implicitConversions

import lms.core, core.Op, core.Name
import lms.core.tree as ast
import lms.runtime.Log
import lms.util.Plumbing.*
import lms.util.CountOrInf, CountOrInf.*

import Pattern.{Var => PVar, Node => PNode}

class EGraph(rules: Ruleset, cfg: EGraph.Config = EGraph.Config()) {
  import EGraph.*
  import ENode.*

  val uf: mutable.ArrayBuffer[EClass] = new mutable.ArrayBuffer
  var dirty: Boolean = false

  private val nodes: mutable.Map[ENode, EClass] = mutable.Map()
  private val classes: mutable.Map[EClass, mutable.Set[ENode]] = mutable.Map()

  private def find(ec: EClass): EClass = {
    val parent = uf(ec.id)
    if parent.id == ec.id then ec
    else {
      val result = find(parent)
      uf(ec.id) = result
      result
    }
  }

  private def union(ai: EClass, bi: EClass): EClass = {
    val a = find(ai)
    val b = find(bi)
    if a == b then a
    else {
      dirty = true
      classes(bi) ++= classes(ai)
      classes(ai) = classes(bi)
      uf(a.id) = b
      b
    }
  }

  private def equals(a: EClass, b: EClass): Boolean = find(a) == find(b)

  private def canonicalize(enode: ENode): ENode = enode match {
    case Node(op, children) => Node(op, children.map(find))
    case Var(name)          => Var(name)
  }

  private def isCanonical(a: EClass): Boolean = uf(a.id) == a
  private def isCanonical(node: ENode): Boolean =
    node match {
      case Node(op, children) => children.forall { isCanonical(_) }
      case Var(name) => true
    }

  private def ensureClass(cls: EClass): mutable.Set[ENode] =
    classes.get(cls).getOrElse {
      val result = mutable.Set[ENode]()
      classes(cls) = result
      result
    }

  private def add(nodeIn: ENode): EClass = {
    val node = canonicalize(nodeIn)
    nodes.get(node) match {
      case Some(cls) => find(cls)
      case None      => {
        val result = EClass(uf.length)
        uf += result
        nodes(node) = result
        ensureClass(result) += node
        return result
      }
    }
  }

  def addNode(op: Op.Pure, children: Seq[EClass]): EClass = add(Node(op, children))

  def addNamedVar(name: Name): EClass = add(Var(name))
  def addNamedVar(name: String): EClass = addNamedVar(Name.from(name))

  private def nodesInClass(cls: EClass): Set[ENode] =
    classes(find(cls)).toSet

  private def repair(node: ENode, oldCls: EClass): Unit = {
    val cls = find(oldCls)
    val canon = canonicalize(node)

    nodes.get(canon) match {
      case Some(other) => if !equals(cls, other) then union(cls, other)
      case None => {
        nodes(canon) = cls
        classes(cls) += canon
      }
    }
  }

  // Remove nodes that are non-canonical
  def gc(): Unit = {
    val allNodes = nodes.toSeq
    for (node, cls) <- allNodes do {
      if !isCanonical(node) then {
        nodes.remove(node)
        classes(cls).remove(node)
      }
    }
  }

  def rebuild(): Unit = {
    if !dirty then {
      Log.info("skipping rebuild because graph is already clean")
      return
    }
    Log.info("rebuilding")

    val oldNodes = nodes.toSeq

    while dirty do {
      dirty = false
      oldNodes.foreach(repair)
    }

    gc()

    Log.info("rebuilt!")
  }

  private type Subst = Map[String, EClass]

  def ematch(pat: Pattern, cls: EClass): Seq[Subst] = {
    ematchImpl(0, pat, find(cls), Map())
  }

  private def joinSubmatches(
      depth: Int,
      children: Seq[(Pattern, EClass)],
      subst: Subst
  ): Seq[Subst] = children match {
    case Nil                      => Seq(subst)
    case (subpat, subcls) +: rest => for {
        s <- ematchImpl(depth + 1, subpat, subcls, subst)
        s2 <- joinSubmatches(depth, rest, s ++ subst)
      } yield s2 ++ s
  }

  private def matchNode(pat: Pattern, node: ENode): Seq[Subst] = pat match {
    case PVar(name)         => nodes.get(node).map(cls => Map((name, cls))).toSeq
    case PNode(op, subpats) => matchNodeImpl(0, node, op, subpats, Map())
  }

  private def matchNodeImpl(
      depth: Int,
      node: ENode,
      op: core.Op,
      subpats: Seq[Pattern],
      subst: Subst
  ): Seq[Subst] = node match {
    case Node(op2, children) if op == op2 && subpats.length == children.length =>
      joinSubmatches(depth, subpats.zip(children), subst)
    case _ => Seq()
  }

  private def ematchImpl(
      depth: Int,
      pat: Pattern,
      cls: EClass,
      subst: Subst
  ): Seq[Subst] = {
    //Log.debug(s"[depth: $depth] attempting to match $pat at $cls")

    pat match {
      case PVar(name) => subst.get(name) match {
          case Some(ecls) => if equals(cls, ecls) then Seq(subst.toMap) else Seq()
          case None       => Seq(subst + ((name, cls)))
        }

      case PNode(op, subpats) => nodesInClass(cls).toSeq
          .flatMap(matchNodeImpl(depth, _, op, subpats, subst))
    }
  }

  private def buildRHS(subst: Subst, rhs: Pattern): EClass = rhs match {
    case PVar(name)         => subst(name)
    case PNode(op, subpats) => addNode(op, subpats.map(buildRHS(subst, _)))
  }

  def allClasses: Set[EClass] = { uf.toSeq.filter(isCanonical).toSet }

  // returns true when saturated
  def applyRules(): Boolean = {
    val substs = for {
      cls <- allClasses.toSeq if isCanonical(cls)
      case Expansion(lhs, rhs) <- rules.toSeq
      subst <- ematch(lhs, cls)
    } yield (cls, buildRHS(subst, rhs))

    substs.foreach(union)
    return !dirty
  }

  def saturate(): Unit = {
    var saturated = false
    var iterations = 0

    while !saturated && iterations < cfg.maxIterations do {
      Log.info(s"iteration: $iterations")
      iterations += 1
      saturated = applyRules()
      rebuild()
    }
  }

  private case class Best(term: ast.Term, cost: Int)

  private def reachable(root: EClass): Set[EClass] = {
    val seen = mutable.Set.empty[EClass]

    def visit(cls: EClass): Unit = if seen.add(cls) then
      for node <- nodesInClass(cls) do
        node match {
          case Node(_, children) => children.foreach(visit)
          case _                 => ()
        }

    visit(root)
    seen.toSet
  }

  private def candidate(
      node: ENode,
      best: mutable.Map[EClass, Best]
  ): Option[ast.Term] = node match {
    case Var(name)          => Some(ast.V(name))
    case Node(op, children) => {
      val childTerms = children.map(cls => best.get(cls).map(_.term)).toSeq

      if childTerms.forall(_.isDefined) then Some(ast.E(op, childTerms.flatten))
      else None
    }
  }

  // CR-someday cwong:
  // This can actually pessimize some shared structure. If we have something
  // like
  //
  //   let x = e in x + x
  //
  // then this could be extracted as `e + e`, which ~doubles the size of the
  // expression tree (`e` occurs once in the input but twice in the extracted
  // form). To resolve this, we could instead produce let-nodes, which would
  // give output that looks closer to ANF. However, to do so properly, we'd
  // either need the ability to generate fresh names or return an intermediate
  // form that can be filled in with fresh names later.
  private def runExtract(root: EClass): Option[ast.Term] = {
    val classes = reachable(root)
    val best = mutable.Map.empty[EClass, Best]

    var changed = true

    while changed do {
      changed = false
      for cls <- classes do {
        nodesInClass(cls).iterator.flatMap(node => candidate(node, best).iterator)
          .map(term => Best(term, ast.Term.size(term))).minByOption(_.cost).foreach {
            b =>
              best.get(cls) match {
                case None => {
                  best(cls) = b
                  changed = true
                }
                case Some(old) => if b.cost < old.cost then {
                    best(cls) = b
                    changed = true
                  }
              }
          }
      }
    }

    best.get(root).map(_.term)
  }

  def extract(cls: EClass): Option[ast.Term] = {
    rebuild()
    runExtract(cls)
  }

  def debugDump() = {
    val uf2 = uf.toSeq
    for (cls <- allClasses) {
      Log.debug(s"$cls: ")
      for (node <- nodesInClass(cls)) { Log.debug(s"  $node") }
    }
  }
}

object EGraph {
  private enum ENode derives CanEqual {
    case Node(op: Op.Pure, children: Seq[EClass])
    case Var(name: Name)
  }

  case class EClass(id: Int) derives CanEqual
  case class Config(
      maxIterations: CountOrInf = 9,
      maxDepth: CountOrInf = 10,
      namePrefix: String = "x"
  )
}
