package lms.ir.opt

import scala.collection.mutable

import lms.core, core.Op, core.Op.*
import lms.codegen.ast
import lms.runtime.Log
import lms.util.Plumbing.*

import Pattern.{Var => PVar, Node => PNode}

class EGraph(cfg: EGraph.Config = EGraph.Config()) {
  import EGraph.*
  import ENode.*

  val uf: mutable.ArrayBuffer[EClass] = new mutable.ArrayBuffer
  var dirty: Boolean = false

  // Maintained lazily, updated on rebuild
  private val nodes: mutable.Map[ENode, EClass] = mutable.Map()

  // Maintained eagerly, updated on union or addition
  // private val classes: mutable.Map[EClass, mutable.Set[ENode]] = mutable.Map()

  private def find(ec: EClass): EClass = {
    val parent = uf(ec.id)
    if parent.id == ec.id then ec
    else {
      val result = find(parent)
      //uf(ec.id) = result
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
      // classes(b) ++= classes(a)
      // classes(a) = classes(b)
      b
    }
  }

  private def equals(a: EClass, b: EClass): Boolean = find(a) == find(b)

  private def canonicalize(enode: ENode): ENode = enode match {
    case Node(op, children) => Node(op, children.map(find))
    case V(name)            => V(name)
  }

  private def isCanonical(a: EClass): Boolean = uf(a.id) == a
  // private def ensureClass(ec: EClass): mutable.Set[ENode] = classes.getOrElseUpdate(find(ec), mutable.Set())

  private def add(nodeIn: ENode): EClass = {
    val node = canonicalize(nodeIn)
    val result = nodes.get(node) match {
      case Some(cls) => find(cls)
      case None      => {
        val result = EClass(uf.length)
        uf += result
        nodes(node) = result
        // ensureClass(result) += node
        result
      }
    }
    result
  }

  def addNode(op: Op, children: Seq[EClass]): EClass = add(Node(op, children))

  def addVar(name: String): EClass = add(V(name))

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

  private def ematchImpl(
      depth: Int,
      pat: Pattern,
      cls: EClass,
      subst: Subst
  ): Seq[Subst] = {
    Log.info(s"[depth: $depth] attempting to match $pat at $cls")
    Log.info(s"[depth: $depth] subst: $subst")

    pat match {
      case PVar(name) => subst.get(name) match {
          case Some(ecls) => if equals(cls, ecls) then Seq(subst.toMap) else Seq()
          case None       => Seq(subst + ((name, cls)))
        }

      case PNode(op, subpats) => nodesInClass(cls).toSeq.flatMap {
          case Node(op2, children) if op == op2 && subpats.length == children.length =>
            joinSubmatches(depth, subpats.zip(children), subst)
          case _ => Seq()
        }
    }
  }

  private def buildRHS(subst: Subst, rhs: Pattern): EClass = rhs match {
    case PVar(name)         => subst(name)
    case PNode(op, subpats) => addNode(op, subpats.map(buildRHS(subst, _)))
  }

  // returns true when saturated
  def applyRules(rules: Seq[Rule]): Unit = {
    val uf2 = uf.toSeq
    val substs = for {
      cls <- uf2 if isCanonical(cls)
      case Rule(lhs, rhs, oneWay) <- rules
      subst <- ematch(lhs, cls)
    } yield (cls, buildRHS(subst, rhs))

    substs.foreach(union)
  }

  def saturate(rules: Seq[Rule]): Unit = {
    var saturated = false
    var iterations = 0

    while !saturated && iterations < cfg.maxIterations do {
      iterations += 1
      applyRules(rules)
      saturated = !dirty
    }
  }

  private def extractNode(node: ENode): Option[ast.Term] = node match {
    case V(name)            => Some(ast.V(name))
    case Node(op, children) => children.map(extractCls).traverse.map(ast.E(op, _))
  }

  private def extractCls(cls: EClass): Option[ast.Term] = {
    nodesInClass(cls).toSeq.filterMap(extractNode).argmin(ast.Term.size)
  }

  def extract(cls: EClass): Option[ast.Term] = {
    rebuild()
    extractCls(cls)
  }
}

object EGraph {
  private enum ENode derives CanEqual {
    case Node(op: Op, children: Seq[EClass])
    case V(name: String)
  }

  enum CountOrInf derives CanEqual {
    case Count(x: Int)
    case Infinity
  }
  export CountOrInf.*

  given Conversion[Int, CountOrInf] with
    def apply(x: Int): CountOrInf = Count(x)

  extension (x: Int)
    def <(rhs: CountOrInf): Boolean = rhs match {
      case Count(y) => x < y
      case Infinity => true
    }

  given Ordering[CountOrInf] with
    def compare(lhs: CountOrInf, rhs: CountOrInf): Int = (lhs, rhs) match {
      case (Infinity, Infinity) => 0
      case (Infinity, Count(_)) => 1
      case (Count(_), Infinity) => -1
      case (Count(l), Count(r)) => l - r
    }

  case class EClass(id: Int) derives CanEqual
  case class Config(maxIterations: CountOrInf = 100)
}
