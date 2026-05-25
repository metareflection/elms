package elms.test.koika

import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given
import elms.core.StructManifest
import elms.codegen.CCodegen

import elms.test.*

object NanoRisc {
  abstract sealed trait Operand derives CanEqual

  // CR cwong: It's not clear to me how much these newtypes actually help; we
  // just end up peppering the code with `unReg` everywhere.
  case class Imm(i: Int) extends Operand {
    def unImm = i
  }
  case class Reg(i: Int) extends Operand {
    def unReg = i
  }

  case class Addr(i: Int) {
    def unAddr = i
  }

  abstract sealed trait Cmp derives CanEqual
  case object Eq extends Cmp
  case object Ne extends Cmp
  case object Lt extends Cmp
  case object Ge extends Cmp

  abstract sealed trait Op derives CanEqual
  case object Plus extends Op
  case object Sub extends Op
  case object Mul extends Op

  abstract sealed class Instr
  case class Mov(dst: Reg, src: Operand) extends Instr
  case class Binop(op: Op, dst: Reg, src1: Reg, src2: Operand) extends Instr
  case class Load(dst: Reg, src: Reg, offs: Operand) extends Instr
  case class Store(dst: Reg, src: Reg, offs: Operand) extends Instr
  case class B(cmp: Option[(Cmp, Reg, Operand)], tgt: Addr) extends Instr

  trait Ops extends DslOps {
    extension (cmp: Cmp)
      def eval(op1: Rep[Int], op2: Rep[Int]): Rep[Boolean] = cmp match {
        case Eq => op1 === op2
        case Ne => op1 !== op2
        case Lt => op1 < op2
        case Ge => op1 >= op2
      }

    extension (op: Op)
      def eval(op1: Rep[Int], op2: Rep[Int]): Rep[Int] = op match {
        case Plus => op1 + op2
        case Sub  => op1 - op2
        case Mul  => op1 * op2
      }
  }
}

case class StateT(
    regs: Array[Int],
    mem: Array[Int],
    saved_regs: Array[Int],
    cache_keys: Array[Int],
    cache_vals: Array[Int],
    timer: Int
) derives StructManifest

trait StateTOps extends DslOps {
  extension (st: Rep[StateT])
    def regs: Rep[Array[Int]] = st.get("regs").asInstanceOf[Rep[Array[Int]]]
    def mem: Rep[Array[Int]] = st.get("mem").asInstanceOf[Rep[Array[Int]]]
    def saved_regs: Rep[Array[Int]] = st.get("saved_regs").asInstanceOf[Rep[Array[Int]]]
    def cache_keys: Rep[Array[Int]] = st.get("cache_keys").asInstanceOf[Rep[Array[Int]]]
    def cache_vals: Rep[Array[Int]] = st.get("cache_vals").asInstanceOf[Rep[Array[Int]]]
    def timer: Rep[Int] = st.get("timer").asInstanceOf[Rep[Int]]

    def mem_=(v: Rep[Array[Int]]): Rep[Unit] = st.set("mem", v)
    def saved_regs_=(v: Rep[Array[Int]]): Rep[Unit] = st.set("saved_regs", v)
    def cache_keys_=(v: Rep[Array[Int]]): Rep[Unit] = st.set("cache_keys", v)
    def cache_vals_=(v: Rep[Array[Int]]): Rep[Unit] = st.set("cache_vals", v)
    def timer_=(v: Rep[Int]): Rep[Unit] = st.set("timer", v)
}

@virtualize
trait Common extends StateTOps with NanoRisc.Ops {
  import NanoRisc.*

  val prog: Vector[NanoRisc.Instr]
  def useCache: Boolean

  lazy val cache: Array[Option[Rep[StateT => StateT]]] =
    (for (p <- prog) yield None).toArray

  def get_reg(s: Rep[StateT], i: Rep[Int]): Rep[Int]
  def set_reg(s: Rep[StateT], i: Rep[Int], v: Rep[Int]): Rep[Unit]
  def get_mem(s: Rep[StateT], i: Rep[Int]): Rep[Int]
  def set_mem(s: Rep[StateT], i: Rep[Int], v: Rep[Int]): Rep[Unit]

  def tick(s: Rep[StateT]): Rep[Unit] = s.timer = s.timer + 1

  def operand(s: Rep[StateT], vl: Operand): Rep[Int] = vl match {
    case Imm(i) => i
    case Reg(i) => get_reg(s, i)
  }

  // The indirection via `execute` is necessary to generate functions for
  // each instruction.
  def execute(i: Int, s: Rep[StateT]): Rep[StateT]
  def call(i: Int, s: Rep[StateT]): Rep[StateT] =
    if (useCache) {
      if (i < cache.length) {
        val f = cache(i) match {
          case None => {
            val f = fun { (s: Rep[StateT]) => execute(i, s) }
            cache(i) = Some(f)
            f
          }
          case Some(f) => f
        }
        f(s)
      } else { s }
    } else { execute(i, s) }

  def snippet(s: Rep[StateT]): Rep[StateT] = call(0, s)
}

@virtualize
trait Naive extends Common {
  import NanoRisc.*

  override def useCache = true

  override def get_reg(s: Rep[StateT], i: Rep[Int]): Rep[Int] = s.regs(i)
  override def set_reg(s: Rep[StateT], i: Rep[Int], v: Rep[Int]): Rep[Unit] =
    s.regs(i) = v

  override def get_mem(s: Rep[StateT], i: Rep[Int]): Rep[Int] = s.mem(i)
  override def set_mem(s: Rep[StateT], i: Rep[Int], v: Rep[Int]): Rep[Unit] =
    s.mem(i) = v

  def execute(i: Int, s: Rep[StateT]): Rep[StateT] =
    if (i < prog.length) {
      tick(s)
      prog(i) match {
        case Mov(dst, src) => {
          set_reg(s, dst.unReg, operand(s, src))
          call(i + 1, s)
        }
        case Binop(op, dst, src1, src2) => {
          set_reg(s, dst.unReg, op.eval(operand(s, src1), operand(s, src2)))
          call(i + 1, s)
        }
        case Load(dst, src, im) => {
          set_reg(s, dst.unReg, get_mem(s, operand(s, src) + operand(s, im)))
          call(i + 1, s)
        }
        case Store(dst, src, im) => {
          set_mem(s, operand(s, src) + operand(s, im), operand(s, dst))
          call(i + 1, s)
        }
        case B(None, tgt)                    => call(tgt.unAddr, s)
        case B(Some((cmp, src1, src2)), tgt) =>
          if (cmp.eval(operand(s, src1), operand(s, src2))) { call(tgt.unAddr, s) }
          else { call(i + 1, s) }
      }
    } else { s }
}

@virtualize
trait Cached extends Naive {
  def pushLRU(s: Rep[StateT], addr: Rep[Int], v: Rep[Int]): Rep[Unit] = {
    s.cache_keys(1) = s.cache_keys(0)
    s.cache_vals(1) = s.cache_vals(0)

    s.cache_keys(0) = addr
    s.cache_vals(0) = v
  }

  def runCache(s: Rep[StateT], addr: Rep[Int], v: Option[Rep[Int]]): Rep[Int] = {
    if (s.cache_keys(0) === addr) {
      // address is in cache, return value
      v match {
        case Some(x) => {
          s.cache_vals(0) = x
          x
        }
        case None => s.cache_vals(0)
      }
    } else if (s.cache_keys(1) === addr) {
      // key is at tail of LRU queue, so set addr as head
      val result = v match {
        case Some(x) => x
        case None    => s.cache_vals(1)
      }

      pushLRU(s, addr, result)

      s.timer += 1
      result
    } else {
      // address not in cache
      val result = v match {
        case Some(x) => x
        case None    => s.mem(addr)
      }

      // evict LRU and write back to memory
      // CR-soon cwong: Triple-check that this is correct -- I think we might
      // accidentally write back to memory too soon if a speculative
      // instruction evicts an entry.
      s.mem(s.cache_keys(1)) = s.cache_vals(1)

      pushLRU(s, addr, result)

      s.timer += 100

      result
    }
  }

  override def get_mem(s: Rep[StateT], addr: Rep[Int]): Rep[Int] =
    runCache(s, addr, None)

  override def set_mem(s: Rep[StateT], addr: Rep[Int], v: Rep[Int]): Rep[Unit] = {
    runCache(s, addr, Some(v))
    unit(())
  }
}

@virtualize
trait Speculative extends Cached {
  import NanoRisc.*

  given liftable: Liftable[Unit] = summon[Liftable[Unit]]

  val savedRegisters = scala.collection.mutable.Set[Reg]()

  def saveForRollback(s: Rep[StateT], instr: Instr): Rep[Unit] = {
    instr match {
      case Load(rd, _, _) => {
        if (!savedRegisters.contains(rd)) {
          s.saved_regs(rd.unReg) = get_reg(s, rd.unReg)
          savedRegisters += rd
        }
      }
      case _ => ()
    }
    unit(())
  }
  def rollback(s: Rep[StateT]): Rep[Unit] = {
    s.timer += 15
    for (rd <- savedRegisters) { set_reg(s, rd.unReg, s.saved_regs(rd.unReg)) }
    unit(())
  }
  def resetSaved(): Unit = { savedRegisters.clear() }

  var inBranch: Option[B] = None

  override def useCache: Boolean = inBranch == None
  override def execute(i: Int, s: Rep[StateT]): Rep[StateT] = inBranch match {
    case None => (i < prog.length, prog(i)) match {
        case (true, B(Some(cnd), tgt)) if tgt.unAddr > i => {
          inBranch = Some(B(Some(cnd), tgt))
          call(i + 1, s)
        }
        case _ => super.execute(i, s)
      }
    case Some(B(Some((cmp, src1, src2)), tgt)) => {
      if (i == tgt.unAddr) {
        inBranch = None
        if (cmp.eval(operand(s, src1), operand(s, src2))) {
          rollback(s)
          call(tgt.unAddr, s)
        }
        resetSaved()
        s
      } else if (i < prog.length) {
        prog(i) match {
          case Load(rd, rs, imm) if rd != src1 && rd != src2 => {
            saveForRollback(s, Load(rd, rs, imm))
            super.execute(i, s)
          }
          case _ => {
            var result: Rep[StateT] = s
            inBranch = None
            if (cmp.eval(operand(s, src1), operand(s, src2))) {
              rollback(s)
              result = call(tgt.unAddr, s)
            } else { result = super.execute(i, s) }
            resetSaved()
            result
          }
        }
      } else { s }
    }
    case _ => { super.execute(i, s) }
  }
}

abstract class GenericKoikaDriver[A: Typable, B: Typable] extends DslDriver[A, B] {
  override val codegen = CCodegen()

  val num_regs: Int = 8
  val mem_size: Int = 30
  val secret_size: Int = 10
  val secret_offset: Int = 20
  val cache_size: Int = 0

  val stateT: String = "StateT"

  // CR cwong: In theory, this should be derivable from the StateT manifest.
  // However, CBMC doesn't really like dealing with unknown-length arrays, so
  // for now we hardcode this. What we *should* do is introduce some kind of
  // `KnownLengthArray`.
  val stateTDef: String = s"""struct $stateT {
  |  int regs[NUM_REGS];
  |  int mem[MEM_SIZE];
  |  int saved_regs[NUM_REGS];
  |  int cache_keys[CACHE_LRU_SIZE];
  |  int cache_vals[CACHE_LRU_SIZE];
  |  int timer;
  |};""".stripMargin

  val header: String = s"""
#define NUM_REGS $num_regs
#define MEM_SIZE $mem_size
#define SECRET_SIZE $secret_size
#define SECRET_OFFSET $secret_offset
#define CACHE_LRU_SIZE $cache_size
#ifndef CBMC
#define __CPROVER_assert(b,s) 0
#define nondet_uint() 0
#else
int nondet_uint();
#endif
int bounded(int low, int high) {
  int x = nondet_uint();
  __CPROVER_assume(low <= x && x <= high);
  return x;
}
$stateTDef
"""

  val init: String

  val initialize_input: String =
    """
      |  int x = bounded(0, 20);
      |  s1.regs[0] = x;
      |  s2.regs[0] = x;""".stripMargin

  val initialize_secret: String =
    """
      |  // initialize secret
      |  for (int i=0; i<SECRET_SIZE; i++) {
      |    s1.mem[SECRET_OFFSET+i] = bounded(0, 20);
      |    s2.mem[SECRET_OFFSET+i] = bounded(0, 20);
      |  }""".stripMargin

  def main: String =
    s"""int main(int argc, char* argv[]) {
       |  struct $stateT s1, s2;
       |  init(&s1);
       |  init(&s2);
       |  $initialize_input
       |  $initialize_secret
       |  struct $stateT *s1_ = snippet(&s1);
       |  struct $stateT *s2_ = snippet(&s2);
       |  __CPROVER_assert(s1_->timer==s2_->timer, "timing leak");
       |  return 0;
       |}""".stripMargin

  override def code =
    s"""$header
       |$init
       |
       |/*****************************************
       |Emitting C Generated Code
       |*******************************************/
       |
       |${super.code}
       |
       |/*****************************************
       |End of C Generated Code
       |*******************************************/
       |
       |$main""".stripMargin
}
