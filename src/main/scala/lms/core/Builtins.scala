package lms.core

trait Builtins extends PrimitiveOps {
  object Functions {
    val print: Rep[String => Unit] = unsafeDeclare("print")
    val println: Rep[String => Unit] = unsafeDeclare("println")
  }

  object Builtins {
    def print(s: Rep[String]): Rep[Unit] = Functions.print(s)
    def println(s: Rep[String]): Rep[Unit] = Functions.println(s)
  }
}
