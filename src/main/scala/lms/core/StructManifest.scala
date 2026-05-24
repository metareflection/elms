package elms.core

import scala.deriving.Mirror

import elms.core.macros.Manifest

case class STRUCT(repr: StructRepr) extends Type

trait StructRepr {
  def name: String
  def members: Map[String, Type]

  def get(name: String): Option[Type] = members.get(name)
}

trait StructManifest[A] {
  val repr: StructRepr

  def name: String = repr.name
  def members: Map[String, Type] = repr.members

  def getField(name: String): Option[Type] = repr.get(name)
  def contains(name: String): Boolean = getField(name).isDefined
}

object StructManifest {
  inline def derived[A](using Mirror.ProductOf[A]): StructManifest[A] =
    ${ Manifest.derivedImpl[A] }
}

given typStruct[A](using manifest: StructManifest[A]): Typable[A] with
  val identity: Type = STRUCT(manifest.repr)
