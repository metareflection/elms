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
  case object And extends Op
  case object Or extends Op

  case object IfThenElse extends Op
  case object While extends Op

  case object StringLength extends Op
  case object StringTake extends Op
  case object StringDrop extends Op
  case object StringStartsWith extends Op
  case object StringEndsWith extends Op
  case object StringCharAt extends Op
  case object StringSubstring extends Op
}
