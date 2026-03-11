// Contains the operations supported by all LMS backends.

package lms.ir

sealed trait Op

case object App extends Op

case object Plus extends Op
case object Times extends Op
case object Minus extends Op

case object IfThenElse extends Op
case object While extends Op
