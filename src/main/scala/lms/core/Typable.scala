package lms.core

import scala.Conversion

import lms.ir

sealed trait Typable[A] {
  val identity: ir.Type
}

object Typable {
  def ofLiftable[A](x: Liftable[A]): Typable[A] =
    new Typable[A] { val identity = x.identity }
}
