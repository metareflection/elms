package elms.core

import elms.core.Op._

trait VarOps extends Base {
  case class Var[+T]()

  def newVar[T: Typable](initial: Rep[T]): Rep[Var[T]] =
    unsafeReflect(VarNew(summon[Typable[T]].identity), initial)
  def varRead[T](rc: Rep[Var[T]]) = unsafeReflect(VarGet, rc)
  def varWrite[T](rc: Rep[Var[T]], v: Rep[T]): Rep[Unit] = unsafeReflect(VarSet, rc, v)

  extension [T](rc: Rep[Var[T]])
    def get: Rep[T] = varRead(rc)
    def set(v: Rep[T]): Rep[Unit] = varWrite(rc, v)
    infix def :=(v: Rep[T]): Rep[Unit] = rc.set(v)
}
