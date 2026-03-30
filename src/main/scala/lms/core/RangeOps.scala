package lms.core

import lms.core.Op

trait RangeOps extends Base {
  extension (st: Rep[Int])
    def until(end: Rep[Int]): Rep[Range] = unsafeReflect(Op.Range, st, end)

  extension (st: Int)
    def until(end: Int): Range = Range.Exclusive(st, end, 1)
    def until(end: Rep[Int]): Rep[Range] = unsafeReflect(Op.Range, unit(st), end)

  extension (lhs: Rep[Range])
    def start: Rep[Int] = unsafeReflect(Op.RangeStart, lhs)
    def end: Rep[Int] = unsafeReflect(Op.RangeEnd, lhs)

    def foreach(body: Rep[Int] => Rep[Unit]): Unit = {
      // CR-someday cwong: Ideally, we'd want `RangeForEach` to carry the name of
      // its bound variable with it, rather than hiding it as a `variable` child.
      // We have to do it this way due to the fact that `Name` is a type from the
      // `ir.Builder`, rather than being built into `Base`.
      val v: Rep[Int] = unsafeFresh()
      unsafeReflect(Op.RangeForEach, v, lhs.start, lhs.end, region(body(v)))
    }
}
