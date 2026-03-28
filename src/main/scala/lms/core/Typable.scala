package lms.core

import scala.Conversion
import annotation.implicitNotFound

import Type._

@implicitNotFound("${A} is not a DSL type")
sealed abstract class Typable[A] {
  val identity: Type
}

@implicitNotFound("${A} cannot be lifted")
sealed abstract class Liftable[A] extends Typable[A] {
}

object Typable {
  implicit class ArrayManifest[A](using t: Typable[A]) extends Typable[Array[A]] {
    val identity = ARRAY(t.identity)
  }
}

object Liftable {
  implicit object LiftUnit extends Liftable[Unit] {
    val identity = UNIT
  }

  implicit object LiftInt extends Liftable[Int] {
    val identity = INT
  }

  implicit object LiftBool extends Liftable[Boolean] {
    val identity = BOOL
  }

  implicit object LiftChar extends Liftable[Char] {
    val identity = CHAR
  }

  implicit object LiftString extends Liftable[String] {
    val identity = STRING
  }
}
