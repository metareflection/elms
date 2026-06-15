package elms.test

import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given

import elms.core.{Op, Type, Typable}
import elms.codegen.ScalaCodegen

class AutomataTest extends SnapshotFunSuite {
  val under = "nfa/"

  test("findAAB") {
    val p = new AutomataDriver with NFAtoDFA {
      def snippet(x: Rep[Unit]) = {
        def findAAB(): NIO = {
          guard(C('A')) {
            guard(C('A')) {
              guard(C('B'), true) {
                stop()
          }}} ++
          guard(W) { findAAB() } // in parallel ...
        }

        // NFA to DFA conversion via staging explained below.
        convertNFAtoDFA((findAAB(), false))
      }
    }

    // CR cwong: make dynamic loading work!
    /*
    // Some tests.
    assertResult(true){p.matches("AAB")}
    assertResult(false){p.matches("AAC")}
    assertResult(true){p.matches("AACAAB")}
    assertResult(true){p.matches("AACAABAAC")}
    */

    // The generated code for the DFA is shown at the end.
    check("aab", p.code, accept=true)
  }
}

trait NFAOps extends elms.util.ClosureCompare {
  type NIO = List[NTrans] // state: many possible transitions
  def guard(cond: CharSet, found: => Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, () => found, () => e))
  }
  def stop(): NIO = Nil
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, () => false, s))
  def guards(conds: List[CharSet], found: Boolean = false)(e: => NIO): NIO = {
    conds.flatMap(guard(_, found)(e))
  }
  case class NTrans(c: CharSet, e: () => Boolean, s: () => NIO)
      extends Ordered[NTrans] {
    override def compare(o: NTrans) = {
      val i = this.c.compare(o.c)
      if (i != 0) i
      else {
        val i2 = this.e().compare(o.e())
        if (i2 != 0) i2
        else {
          val tf = canonicalize(this.s())
          val of = canonicalize(o.s())
          if (tf == of) 0 else tf.compare(of)
        }
      }
    }
  }

  sealed abstract class CharSet extends Ordered[CharSet] derives CanEqual {
    override def compare(o: CharSet) = (this, o) match {
      case (W, W)         => 0
      case (W, _)         => 1
      case (_, W)         => -1
      case (C(c1), C(c2)) => c1.compare(c2)
    }
  }

  case class C(c: Char) extends CharSet
  case object W extends CharSet
}

case class Automaton[@specialized(Char) I, @specialized(Boolean) O](
    out: O,
    next: I => Automaton[I, O]
)

type DfaState = Automaton[Char, Boolean]
case object DFAStateT extends Type
given Typable[DfaState] with {
  val identity = DFAStateT
}

trait DFAOps extends DslOps {
  type DIO = Rep[DfaState]
  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(false)(f)

  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO =
    unsafeReflect(Op.Custom("Automaton", DFAStateT), e, lam(f))
}

@virtualize
trait NFAtoDFA extends NFAOps with DFAOps {

  /** Translating an NFA to a DFA is accomplished by creating a DFA state for each
    * encountered NFA configuration (removing duplicate states via `canonicalize`):
    */
  def convertNFAtoDFA(in: (NIO, Boolean)): DIO = {
    def iterate(flag: Boolean, state: NIO): DIO = {
      dfa_trans(flag) { (c: Rep[Char]) =>
        exploreNFA(canonicalize(state), c) { iterate }
      }
    }
    iterate(in._2, in._1)
  }

  def canonicalize(state: NIO): NIO = {
    if (state.isEmpty) state
    else {
      val state_sorted = state.sorted
      state_sorted.head ::
        (for ((s, sn) <- (state_sorted zip state_sorted.tail) if s.compare(sn) != 0)
          yield sn)
    }
  }

  def exploreNFA[A](xs: NIO, cin: Rep[Char])(k: (Boolean, NIO) => Rep[A]): Rep[A] =
    xs match {
      case Nil                     => k(false, Nil)
      case NTrans(W, e, s) :: rest =>
        val (xs1, xs2) = xs.partition(_.c != W)
        exploreNFA(xs1, cin)((flag, acc) =>
          k(flag || xs2.exists(_.e()), acc ++ xs2.flatMap(_.s()))
        )
      case NTrans(cset, e, s) :: rest =>
        if (cset.contains(cin)) {
          val xs1 = for (NTrans(rcset, re, rs) <- rest; kcset <- rcset.knowing(cset))
            yield NTrans(kcset, re, rs)
          exploreNFA(xs1, cin)((flag, acc) => k(flag || e(), acc ++ s()))
        } else {
          val xs1 =
            for (NTrans(rcset, re, rs) <- rest; kcset <- rcset.knowing_not(cset))
              yield NTrans(kcset, re, rs)
          exploreNFA(xs1, cin)(k)
        }
    }

  extension (s1: CharSet)
    def contains(c: Rep[Char]): Rep[Boolean] = s1 match {
      case C(c1) => c === c1
      case W     => unit(true)
    }
    def knowing(s2: CharSet): Option[CharSet] = (s1, s2) match {
      case (W, _)                     => Some(W)
      case (C(c1), C(c2)) if c1 == c2 => Some(W)
      case _                          => None
    }
    def knowing_not(s2: CharSet): Option[CharSet] = (s1, s2) match {
      case (C(c1), C(c2)) if c1 == c2 => None
      case _                          => Some(s1)
    }
}

abstract class AutomataDriver
    extends SimpleSnippetDriver[Unit, Automaton[Char, Boolean]] with DFAOps {
  override val codegen = new ScalaCodegen {
    override protected def renderType(ty: Type): String = ty match {
      case DFAStateT => "Automaton[Char, Boolean]"
      case _ => super.renderType(ty)
    }
  }
}
