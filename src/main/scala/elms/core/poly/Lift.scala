package elms.core.poly

import scala.Conversion
import annotation.implicitNotFound

import elms.core.{Primitive, Typable}

trait Lift extends HasRep {
  @implicitNotFound("${A} cannot be lifted")
  abstract class Liftable[A: Typable] {
    def lift(x: A): Rep[A]
  }

  def unit[A: Liftable](x: A): Rep[A] = summon[Liftable[A]].lift(x)

  given [A](using w: Liftable[A]): Conversion[A, Rep[A]] with
    def apply(x: A): Rep[A] = unit(x)

  given liftablePrimitive[A: Primitive](using Typable[A]): Liftable[A]
}
