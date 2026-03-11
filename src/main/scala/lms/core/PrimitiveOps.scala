package lms.core

trait PrimitiveOps extends Base {
  def ifThenElse[T](c: Rep[Boolean], t: Rep[T], e: Rep[T]): Rep[T]
}
