package elms.util

import scala.language.implicitConversions

enum CountOrInf derives CanEqual {
  case Count(x: Int)
  case Infinity
}

object CountOrInf {
  given Conversion[Int, CountOrInf] with
    def apply(x: Int): CountOrInf = Count(x)

  extension (x: Int)
    def <(rhs: CountOrInf): Boolean = order.compare(x, rhs) < 0
    def <=(rhs: CountOrInf): Boolean = order.compare(x, rhs) <= 0
    def >(rhs: CountOrInf): Boolean = order.compare(x, rhs) > 0
    def >=(rhs: CountOrInf): Boolean = order.compare(x, rhs) >= 0

  given order: Ordering[CountOrInf] with
    def compare(lhs: CountOrInf, rhs: CountOrInf): Int = (lhs, rhs) match {
      case (Infinity, Infinity) => 0
      case (Infinity, Count(_)) => 1
      case (Count(_), Infinity) => -1
      case (Count(l), Count(r)) => l - r
    }
}
