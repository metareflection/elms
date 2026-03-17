package lms.core

import lms.ir.IRType
import lms.ir.IRType._

// Mappings from base Scala types to IR types. A type `A` has an instance of
// `DslType[A]` if we can lift a value of type `A` into the graph as a
// constant.

trait DslType[A] {
  val identity: IRType { type T = A }
}

implicit object IntW extends DslType[Int] {
  val identity = INT
}

implicit object BoolW extends DslType[Boolean] {
  val identity = BOOL
}
