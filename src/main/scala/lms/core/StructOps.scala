package elms.core

import elms.runtime.Log

// These are low-level operations that simply reflect the operation into the
// IR. If you want more strongly-typed getters and setters, please use a wrapper
// with `asInstanceOf`.

trait StructOps extends Base {
  def checkField[T](field: String)(using manifest: StructManifest[T]): Unit =
    if !manifest.contains(field) then {
      Log.warning(s"Manifest for type ${manifest.name} does not contain field $field")
    }

  def structGet[T: StructManifest](receiver: Rep[T], field: String): Rep[Any] = {
    checkField[T](field)
    unsafeReflect(Op.StructGet(summon[StructManifest[T]].repr, field), receiver)
  }

  def structSet[T: StructManifest](receiver: Rep[T], field: String, v: Rep[Any]): Rep[Unit] = {
    checkField[T](field)
    unsafeReflect(Op.StructSet(field), receiver, v)
  }

  extension [T: StructManifest](t: Rep[T])
    def get(field: String): Rep[Any] = structGet(t, field)
    def set(field: String, v: Rep[Any]): Rep[Unit] = structSet(t, field, v)
}
