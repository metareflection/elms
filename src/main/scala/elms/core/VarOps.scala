package elms.core

import elms.core.Op._

trait VarOps extends Base with poly.VarOps {
  def newVar[T: Typable](initial: Rep[T]): Rep[Var[T]] =
    unsafeReflect(VarNew(summon[Typable[T]].identity), initial)
  def varRead[T](rc: Rep[Var[T]]): Rep[T] = unsafeReflect(VarGet, rc)
  def varWrite[T](rc: Rep[Var[T]], v: Rep[T]): Rep[Unit] = unsafeReflect(VarSet, rc, v)
}
