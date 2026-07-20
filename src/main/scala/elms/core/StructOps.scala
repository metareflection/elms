package elms.core

// These are low-level operations that simply reflect the operation into the
// IR. If you want more strongly-typed getters and setters, please use a wrapper
// with `asInstanceOf`.

trait StructOps extends Base with poly.StructOps {
  def structGet[T: StructManifest](receiver: Rep[T], field: String): Rep[Any] = {
    checkField[T](field)
    unsafeReflect(Op.StructGet(summon[StructManifest[T]].repr, field), receiver)
  }

  def structSet[T: StructManifest](receiver: Rep[T], field: String, v: Rep[Any]): Rep[Unit] = {
    checkField[T](field)
    unsafeReflect(Op.StructSet(field), receiver, v)
  }
}
