package elms.core.poly

import scala.Conversion

import elms.core.Typable

trait VarOps extends HasRep {
  type Var[T]

  def newVar[T: Typable](initial: Rep[T]): Rep[Var[T]]
  def varRead[T](rc: Rep[Var[T]]): Rep[T]
  def varWrite[T](rc: Rep[Var[T]], v: Rep[T]): Rep[Unit]

  extension [T](rc: Rep[Var[T]])
    def get: Rep[T] = varRead(rc)
    def set(v: Rep[T]): Rep[Unit] = varWrite(rc, v)
    infix def :=(v: Rep[T]): Rep[Unit] = rc.set(v)

  given [T]: Conversion[Rep[Var[T]], Rep[T]] with
    def apply(rc: Rep[Var[T]]): Rep[T] = rc.get
}
