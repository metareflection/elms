package elms.core.poly

trait HasRep {
  type Rep[+T] <: elms.core.__Virtualized[T]
}
