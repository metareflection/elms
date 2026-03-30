package lms.core

import scala.Conversion
import scala.util.NotGiven
import annotation.implicitNotFound

import Type._

@implicitNotFound("${A} is not a DSL type")
sealed abstract class Typable[A] {
  val identity: Type
}

@implicitNotFound("${A} cannot be lifted")
sealed abstract class Liftable[A] extends Typable[A]

object Typable {
  given [A](using inner: Typable[A], ng: NotGiven[Liftable[A]]): Typable[Array[A]] with
    val identity = ARRAY(inner.identity)
}

object Liftable {
  given Liftable[Unit] with
    val identity = UNIT

  given Liftable[Int] with
    val identity = INT

  given Liftable[Boolean] with
    val identity = BOOL

  given Liftable[Char] with
    val identity = CHAR

  given Liftable[String] with
    val identity = STRING

  // CR cwong: This is a huge footgun when it comes to pretty-printing `Const`
  // arrays. We should instead introduce a bespoke array constructor that lifts
  // `Array[A]` to `Rep[Array[A]]` elementwise.
  given [A: Liftable]: Liftable[Array[A]] with
    val identity = ARRAY(summon[Liftable[A]].identity)
}
