package lms.core

import lms.ir, ir.Type._

trait Liftable[A] {
  val identity: ir.Type { type T = A }
}

implicit object LiftInt extends Liftable[Int] {
  val identity = INT
}

implicit object LiftBool extends Liftable[Boolean] {
  val identity = BOOL
}
