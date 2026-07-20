package elms.core.poly

trait IntegerOps extends HasRep {
  extension (lhs: Rep[Int])
    def +(rhs: Rep[Int]): Rep[Int]
    def -(rhs: Rep[Int]): Rep[Int]
    def *(rhs: Rep[Int]): Rep[Int]
    def <(rhs: Rep[Int]): Rep[Boolean]
    def >(rhs: Rep[Int]): Rep[Boolean]
    def <=(rhs: Rep[Int]): Rep[Boolean]
    def >=(rhs: Rep[Int]): Rep[Boolean]
}
