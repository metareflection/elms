package lms.core

import annotation.implicitNotFound

trait Type derives CanEqual

case class ARRAY(inner: Type) extends Type
case class ARROW(args: Seq[Type], out: Type) extends Type

sealed trait Primitive[A] extends Type {
  def is[B](other: Primitive[B]): Option[A =:= B]
}

case object UNIT extends Primitive[Unit] {
  def is[B](other: Primitive[B]): Option[Unit =:= B] = other match {
    case UNIT => Some(summon[Unit =:= Unit])
    case _ => None
  }
}

case object INT extends Primitive[Int] {
  def is[B](other: Primitive[B]): Option[Int =:= B] = other match {
    case INT => Some(summon[Int =:= Int])
    case _ => None
  }
}

case object BOOL extends Primitive[Boolean] {
  def is[B](other: Primitive[B]): Option[Boolean =:= B] = other match {
    case BOOL => Some(summon[Boolean =:= Boolean])
    case _ => None
  }
}

case object CHAR extends Primitive[Char] {
  def is[B](other: Primitive[B]): Option[Char =:= B] = other match {
    case CHAR => Some(summon[Char =:= Char])
    case _ => None
  }
}

case object STRING extends Primitive[String] {
  def is[B](other: Primitive[B]): Option[String =:= B] = other match {
    case STRING => Some(summon[String =:= String])
    case _ => None
  }
}

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

  given [A](using prim: Primitive[A]): Typable[A] with
    val identity = prim

  given [A](using inner: Typable[A]): Typable[Array[A]] with
    val identity = ARRAY(inner.identity)
}
