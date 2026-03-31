package lms.core

enum Type derives CanEqual {
  case UNIT
  case INT
  case BOOL
  case CHAR
  case STRING
  case ARRAY(elt: Type)
}
