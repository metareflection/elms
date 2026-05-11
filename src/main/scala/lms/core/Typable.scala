package lms.core

import annotation.implicitNotFound

import Type._

@implicitNotFound("${A} is not a DSL type")
abstract class Typable[A] {
  val identity: Type
}

object Typable {
  given Typable[Unit] with
    val identity = UNIT

  given Typable[Int] with
    val identity = INT

  given Typable[Boolean] with
    val identity = BOOL

  given Typable[Char] with
    val identity = CHAR

  given Typable[String] with
    val identity = STRING

  given [A](using inner: Typable[A]): Typable[Array[A]] with
    val identity = ARRAY(inner.identity)
}
