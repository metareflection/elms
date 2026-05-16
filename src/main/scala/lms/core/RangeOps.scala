package lms.core

import lms.core.Op
import lms.core.Name
import lms.core.Givens.given

trait RangeOps extends PrimitiveOps {
  extension (st: Rep[Int])
    def until(end: Rep[Int]): Rep[Range] = unsafeReflect(Op.Range, st, end)

  extension (st: Int)
    def until(end: Int): Range = Range.Exclusive(st, end, 1)
    def until(end: Rep[Int]): Rep[Range] = unsafeReflect(Op.Range, unit(st), end)

  extension (lhs: Rep[Range])
    def start: Rep[Int] = unsafeReflect(Op.RangeStart, lhs)
    def end: Rep[Int] = unsafeReflect(Op.RangeEnd, lhs)

    def foreach(body: Rep[Int] => Rep[Unit]): Unit = {
      unsafeWithFresh { (name: Name, v: Rep[Int]) =>
        unsafeReflect(Op.RangeForEach(name), lhs.start, lhs.end, region(body(v)))
      }
    }
}
