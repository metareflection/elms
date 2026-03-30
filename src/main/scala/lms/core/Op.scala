// Contains the operations supported by LMS.

package lms.core

sealed trait Op

object Op {
  case class Const[T](val v: T) extends Op

  case object App extends Op

  case object Plus extends Op
  case object Minus extends Op
  case object Times extends Op

  case object Equals extends Op
  case object Lt extends Op
  case object Gt extends Op
  case object Le extends Op
  case object Ge extends Op

  case object And extends Op
  case object Or extends Op

  case object Range extends Op
  case object RangeForEach extends Op
  case object RangeStart extends Op
  case object RangeEnd extends Op

  case object IfThenElse extends Op
  case object While extends Op

  case class ArrayNew(typ: Type) extends Op
  case object ArrayGet extends Op
  case object ArraySet extends Op
  case object ArrayLength extends Op
}
