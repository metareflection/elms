package lms.core

import scala.language.implicitConversions
import scala.Conversion

import lms.ir.Op

trait Base {
  type Rep[T]
  protected type Exp

  def unit[A:DslType](x: A): Rep[A]
  def fun[A,B](f: Rep[A] => Rep[B]): Rep[A => B]

  given [A](using w: DslType[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegisterNode[T](op: Op, children: Vector[Exp]): Exp
}
