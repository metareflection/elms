package elms.core

trait Builtins extends PrimitiveOps {
  object Builtins {
    def print[A](s: Rep[A]): Rep[Unit] = unsafeReflect(Op.Print, s)
    def println[A](s: Rep[A]): Rep[Unit] = unsafeReflect(Op.Println, s)
  }
}
