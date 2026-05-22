package elms.test.tutorial

import scala.language.implicitConversions

import elms.prelude.*
import elms.prelude.given

import elms.test.*

@virtualize
trait Ackermann extends DslOps {
  def a(m: Int): Rep[Int => Int] = fun { (n: Rep[Int]) =>
    if m == 0 then n + 1 else if n === 0 then a(m - 1)(1) else a(m - 1)(a(m)(n - 1))
  }
}

class AckermannTest extends SnapshotFunSuite {
  val under = "tutorial/ack-"

  def specialize(m: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with Ackermann {
    def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
  }

  test("specialize ackermann to m=2") {
    val ack2 = specialize(2)
    check("m2", ack2.code)
  }
}
