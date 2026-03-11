package lms

import lms.ir.IRType
import lms.ir.IRType._

package object core {
  trait DslType[A] {
    val identity: IRType { type T = A }
  }

  implicit object IntW extends DslType[Int] {
    val identity = INT
  }

  implicit object BoolW extends DslType[Boolean] {
    val identity = BOOL
  }
}
