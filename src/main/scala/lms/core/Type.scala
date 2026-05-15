package lms.core

import annotation.implicitNotFound

trait Type derives CanEqual

case class ARRAY(inner: Type) extends Type

sealed trait Primitive[A] extends Type

case object UNIT extends Primitive[Unit]
case object INT extends Primitive[Int]
case object BOOL extends Primitive[Boolean]
case object CHAR extends Primitive[Char]
case object STRING extends Primitive[String]

@implicitNotFound("${A} is not a DSL type")
abstract class Typable[A] {
  val identity: Type
}

object Givens {
  given Primitive[Unit] = UNIT
  given Primitive[Int] = INT
  given Primitive[Boolean] = BOOL
  given Primitive[Char] = CHAR
  given Primitive[String] = STRING

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
