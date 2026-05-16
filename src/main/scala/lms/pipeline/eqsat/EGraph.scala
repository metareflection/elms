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

  // Maintained lazily, updated on rebuild
  private val nodes: mutable.Map[ENode, EClass] = mutable.Map()

  private def find(ec: EClass): EClass = {
    val parent = uf(ec.id)
    if parent.id == ec.id then ec
    else {
      val result = find(parent)
      result
    }
  }

  private def union(ai: EClass, bi: EClass): EClass = {
    val a = find(ai)
    val b = find(bi)
    if a == b then a
    else {
      dirty = true
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

  private def add(nodeIn: ENode): EClass = {
    val node = canonicalize(nodeIn)
    nodes.get(node) match {
      case Some(cls) => find(cls)
      case None      => {
        val result = EClass(uf.length)
        uf += result
        nodes(node) = result
        // ensureClass(result) += node
        return result
      }
    }
  }

  def addNode(op: Op.Pure, children: Seq[EClass]): EClass = add(Node(op, children))

  def addNamedVar(name: Name): EClass = add(Var(name))
  def addNamedVar(name: String): EClass = addNamedVar(Name.from(name))

  private def nodesInClass(cls: EClass): Set[ENode] =
    // classes(find(cls)).toSet
    nodes.toSeq.filterMap { case (node, ncls) =>
      if equals(cls, ncls) then Some(node) else None
    }.toSet

  def rebuild(): Unit = {
    if !dirty then {
      Log.info("skipping rebuild because graph is already clean")
      return
    }
    Log.info("rebuilding")

    val oldNodes = nodes.toSeq

    while dirty do {
      dirty = false
      oldNodes.foreach { case (node, cls) =>
        val newCls = add(node)
        union(cls, newCls)
      }
    }

    Log.info("rebuilt!")
  }

  private type Subst = Map[String, EClass]

  def ematch(pat: Pattern, cls: EClass): Seq[Subst] = {
    rebuild()
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
    Log.info(s"[depth: $depth] attempting to match $pat at $cls")

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
  def applyRules(): Unit = {
    val substs = for {
      cls <- allClasses.toSeq if isCanonical(cls)
      case Expansion(lhs, rhs) <- rules.toSeq
      subst <- ematch(lhs, cls)
    } yield (cls, buildRHS(subst, rhs))

    substs.foreach(union)
  }

  def saturate(): Unit = {
    var saturated = false
    var iterations = 0

    while !saturated && iterations < cfg.maxIterations do {
      iterations += 1
      applyRules()
      saturated = !dirty
    }
  }

  private def extractNode(node: ENode): Option[ast.Term] = node match {
    case Var(name)          => Some(ast.V(name))
    case Node(op, children) => children.map(extractCls).traverse.map(ast.E(op, _))
  }

  private def extractCls(cls: EClass): Option[ast.Term] = {
    nodesInClass(cls).toSeq.filterMap(extractNode).argmin(ast.Term.size)
  }

  def extract(cls: EClass): Option[ast.Term] = {
    rebuild()
    extractCls(cls)
  }

  def debugPrint() = {
    val uf2 = uf.toSeq
    for (cls <- allClasses) {
      println(s"$cls: ")
      for (node <- nodesInClass(cls)) { println(s"  $node") }
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
      maxIterations: CountOrInf = 100,
      maxDepth: CountOrInf = 10,
      namePrefix: String = "x"
  )
}
