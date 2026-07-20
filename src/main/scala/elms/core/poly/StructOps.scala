package elms.core.poly

import elms.runtime.Log

import elms.core.StructManifest

// These are low-level operations that simply reflect the operation into the
// IR. If you want more strongly-typed getters and setters, please use a wrapper
// with `asInstanceOf`.

trait StructOps extends HasRep {
  def checkField[T](field: String)(using manifest: StructManifest[T]): Unit =
    if !manifest.contains(field) then {
      Log.warning(s"Manifest for type ${manifest.name} does not contain field $field")
    }

  def structGet[T: StructManifest](receiver: Rep[T], field: String): Rep[Any]

  def structSet[T: StructManifest](
      receiver: Rep[T],
      field: String,
      v: Rep[Any]
  ): Rep[Unit]

  extension [T: StructManifest](t: Rep[T])
    def get(field: String): Rep[Any] = structGet(t, field)
    def set(field: String, v: Rep[Any]): Rep[Unit] = structSet(t, field, v)
}
