package elms.core.poly

trait BooleanOps extends HasRep {
  extension (lhs: Rep[Boolean])
    def &&(rhs: => Rep[Boolean]): Rep[Boolean]
    def ||(rhs: => Rep[Boolean]): Rep[Boolean]
    def unary_! : Rep[Boolean]
}
