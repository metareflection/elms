// Contains the operations supported by LMS.

package lms.core

enum Op derives CanEqual {
  case Const[T](val v: T)

  case App

  case Plus
  case Minus
  case Times

  case Equals
  case Lt
  case Gt
  case Le
  case Ge

  case And
  case Or

  case Range
  case RangeForEach
  case RangeStart
  case RangeEnd

  case IfThenElse
  case While

  case ArrayNew(typ: Type)
  case ArrayGet
  case ArraySet
  case ArrayLength
}
