package lms.ir

sealed trait IRType {
  type T
}

object IRType {
  case object INT extends IRType {
    type T = Int
  }
  case object BOOL extends IRType {
    type T = Boolean
  }
}
