// Contains the operations supported by all LMS backends.

package lms.core

sealed trait Op

object Op {
  case class Const(typ: Type)(val v: typ.T) extends Op

  case object App extends Op

  case object Plus extends Op
  case object Minus extends Op
  case object Times extends Op

  case object Equals extends Op
  case object And extends Op
  case object Or extends Op

  case object IfThenElse extends Op
  case object While extends Op
}
