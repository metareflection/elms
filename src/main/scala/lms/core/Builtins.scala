package elms.core

trait Builtins extends PrimitiveOps {
  object Functions {
    def print[A]: Rep[A => Unit] = unsafeDeclare("print")
    def println[A]: Rep[A => Unit] = unsafeDeclare("println")
  }

  object Builtins {
    def print[A](s: Rep[A]): Rep[Unit] = Functions.print(s)
    def println[A](s: Rep[A]): Rep[Unit] = Functions.println(s)
  }
}
