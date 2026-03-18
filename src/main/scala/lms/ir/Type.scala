package lms.ir

sealed trait Type {
  type T
}

object Type {
  case object INT extends Type {
    type T = Int
  }
  case object BOOL extends Type {
    type T = Boolean
  }
}
