package elms.core

import elms.core.Op._

trait PrimitiveOps extends Base with poly.PrimitiveOps {
  def __ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] =
    unsafeReflect(IfThenElse, c, region(t), region(e))

  def __whileDo(guard: => Rep[Boolean], body: => Rep[Unit]): Rep[Unit] =
    unsafeReflect(While, region(guard), region(body))

  extension [B](f: Rep[() => B]) def apply(): Rep[B] = unsafeReflect(App, f)

  given repApply1Fun[A, B]: RepApply1[A => B, A, B] with
    def run(f: Rep[A => B], arg: Rep[A]): Rep[B] = unsafeReflect(App, f, arg)

  extension [A1, A2, B](f: Rep[(A1, A2) => B])
    def apply(a1: Rep[A1], a2: Rep[A2]): Rep[B] = unsafeReflect(App, f, a1, a2)

  extension [A1, A2, A3, B](f: Rep[(A1, A2, A3) => B])
    def apply(a1: Rep[A1], a2: Rep[A2], a3: Rep[A3]): Rep[B] =
      unsafeReflect(App, f, a1, a2, a3)

  given liftablePrimitive[A: Primitive](using Typable[A]): Liftable[A] with
    def lift(x: A): Rep[A] = unsafeReflect(Const(x))
}
