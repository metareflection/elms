// Contains the operations supported by LMS.

package lms.core

sealed trait Op(protected val kind: Op.Kind) derives CanEqual

object Op {
  protected enum Kind {
    case Pure
    case Control
    case Effect
  }

  sealed abstract class Pure extends Op(Kind.Pure)
  sealed abstract class Effectful extends Op(Kind.Effect)
  sealed abstract class Control extends Op(Kind.Pure)

  case class Const[T](val v: T) extends Pure

  case object App extends Effectful

  case object Negate extends Pure
  case object Plus extends Pure
  case object Minus extends Pure
  case object Times extends Pure

  case object Equals extends Pure
  case object Lt extends Pure
  case object Gt extends Pure
  case object Le extends Pure
  case object Ge extends Pure

  case object And extends Pure
  case object Or extends Pure

  case object Range extends Pure
  case object RangeForEach extends Control
  case object RangeStart extends Pure
  case object RangeEnd extends Pure

  case object IfThenElse extends Control

  case class ArrayNew(val typ: Type) extends Effectful
  case object ArrayGet extends Effectful
  case object ArraySet extends Effectful
  case object ArrayLength extends Pure
}
