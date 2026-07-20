package elms.core.poly

import scala.Conversion

import elms.runtime.*

trait PrimitiveOps extends Lift {
  def __ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T]

  def __whileDo(guard: => Rep[Boolean], body: => Rep[Unit]): Rep[Unit]

  given __virtualizedBoolConvInternal: Conversion[Rep[Boolean], Boolean] with
    def apply(x: Rep[Boolean]) = {
      throw new LMSRuntimeException(
        "attempted to call __virtualizedBoolConvInternal (did you forget to virtualize?)"
      );
    }

  trait RepLength[A] {
    def run(x: Rep[A]): Rep[Int]
  }

  extension [A](x: Rep[A])(using provider: RepLength[A])
    def length: Rep[Int] = provider.run(x)

  trait RepApply1[F, Input, Output] {
    def run(f: Rep[F], x: Rep[Input]): Rep[Output]
  }

  extension [F, Input, Output](f: Rep[F])(using provider: RepApply1[F, Input, Output])
    def apply(arg: Rep[Input]): Rep[Output] = provider.run(f, arg)

  extension [B](f: Rep[() => B]) def apply(): Rep[B]

  given repApply1Fun[A, B]: RepApply1[A => B, A, B]

  extension [A1, A2, B](f: Rep[(A1, A2) => B])
    def apply(a1: Rep[A1], a2: Rep[A2]): Rep[B]

  extension [A1, A2, A3, B](f: Rep[(A1, A2, A3) => B])
    def apply(a1: Rep[A1], a2: Rep[A2], a3: Rep[A3]): Rep[B]
}
