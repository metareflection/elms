package lms.core

import lms.core.Op._

trait ArrayOps extends PrimitiveOps {
  def newArray[A: Typable](i: Rep[Int]): Rep[Array[A]] =
    unsafeReflect(ArrayNew(summon[Typable[A]].identity), i)

  def initFrom[A: Typable](entries: Seq[A]): Rep[Array[A]] = ???

  extension [A](arr: Rep[Array[A]])
    def get(i: Rep[Int]): Rep[A] = unsafeReflect(ArrayGet, arr, i)
    def set(i: Rep[Int], x: Rep[A]): Rep[Unit] = unsafeReflect(ArraySet, arr, i, x)
    def update(i: Rep[Int], x: Rep[A]): Rep[Unit] = arr.set(i, x)

  given [A]: RepApply1[Array[A], Int, A] with
    def run(arr: Rep[Array[A]], i: Rep[Int]): Rep[A] = arr.get(i)

  given [A]: RepLength[Array[A]] with
    def run(arr: Rep[Array[A]]): Rep[Int] = unsafeReflect(ArrayLength, arr)
}
