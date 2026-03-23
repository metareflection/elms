package lms.core

import scala.Conversion

import Type._

sealed abstract class Typable[A] {
  val identity: Type
}

sealed abstract class Liftable[A] extends Typable[A] {
  val identity: Type { type T = A }
}

implicit object LiftInt extends Liftable[Int] {
  val identity = INT
}

implicit object LiftBool extends Liftable[Boolean] {
  val identity = BOOL
}
