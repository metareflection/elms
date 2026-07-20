package elms.core

import elms.core.Op._

trait ArrayOps extends PrimitiveOps with poly.ArrayOps {
  def newArray[A: Typable](i: Rep[Int]): Rep[Array[A]] =
    unsafeReflect(ArrayNew(summon[Typable[A]].identity), i)

  def initFrom[A: Typable](entries: Seq[A]): Rep[Array[A]] = ???

  extension [A](arr: Rep[Array[A]])
    def get(i: Rep[Int]): Rep[A] = unsafeReflect(ArrayGet, arr, i)
    def set(i: Rep[Int], x: Rep[A]): Rep[Unit] = unsafeReflect(ArraySet, arr, i, x)

  given arrayLength[A]: RepLength[Array[A]] with
    def run(arr: Rep[Array[A]]): Rep[Int] = unsafeReflect(ArrayLength, arr)
}
