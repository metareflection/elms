package lms.core

import lms.core.Op._
import lms.util.OverloadHack._

trait ArrayOps extends Base {
  def newArray[A: Typable](i: Rep[Int]): Rep[Array[A]] =
    unsafeReflect(ArrayNew(summon[Typable[A]].identity), i)

  extension [A](arr: Rep[Array[A]])
    def get(i: Rep[Int]): Rep[A] = unsafeReflect(ArrayGet, arr, i)
    def apply(i: Rep[Int])(using O3): Rep[A] = arr.get(i)
    def set(i: Rep[Int], x: Rep[A]): Rep[Unit] = unsafeReflect(ArraySet, arr, i, x)
    def update(i: Rep[Int], x: Rep[A]): Rep[Unit] = arr.set(i, x)
}
