package lms.core

// CR cwong: This design needs to be rethought.

sealed trait Type

object Type {
  case object UNIT extends Type
  case object INT extends Type
  case object BOOL extends Type
  case object CHAR extends Type
  case object STRING extends Type
  case class ARRAY(elt: Type) extends Type
}
