package elms.core

// This is necessary due to an annoying compiler restriction that prevents
// exporting all givens from a package (rather than from a concrete object).

object instances {
  given Primitive[Unit] = primUnit
  given Primitive[Int] = primInt
  given Primitive[Boolean] = primBool
  given Primitive[Char] = primChar
  given Primitive[String] = primString
  given [A: Primitive]: Typable[A] = typPrim
  given [A: Typable]: Typable[Array[A]] = typArray

  given [A: Primitive]: AsStaticData[A] = staticPrim
  given [A](using AsStaticData[A], Typable[A]): AsStaticData[Array[A]] = staticArray

  given [A: StructManifest]: Typable[A] = typStruct
}
