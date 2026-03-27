package lms.core

sealed trait Type {
  type T
}

object Type {
  case object UNIT extends Type {
    type T = Unit
  }
  case object INT extends Type {
    type T = Int
  }
  case object BOOL extends Type {
    type T = Boolean
  }
  case object CHAR extends Type {
    type T = Char
  }
  case object STRING extends Type {
    type T = String
  }
}
