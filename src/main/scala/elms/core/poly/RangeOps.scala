package elms.core.poly

import elms.core.given

trait RangeOps extends PrimitiveOps {
  extension (st: Rep[Int]) def until(end: Rep[Int]): Rep[Range]

  extension (st: Int)
    def until(end: Int): Range = Range.Exclusive(st, end, 1)
    def until(end: Rep[Int]): Rep[Range] = unit(st).until(end)

  extension (lhs: Rep[Range])
    def start: Rep[Int]
    def end: Rep[Int]

    def foreach(body: Rep[Int] => Rep[Unit]): Unit
}
