package elms.core

sealed trait StaticData {
  def ty: Type
}

case class Scalar[A](x: A)(using val prim: Primitive[A]) extends StaticData {
  def ty = summon[Primitive[A]]
}

case class SArray(elemTy: Type, elems: Seq[StaticData]) extends StaticData {
  def ty = ARRAY(elemTy)
}

trait AsStaticData[A: Typable] {
  def asStaticData(a: A): StaticData
}

extension [A:AsStaticData](data: A)
  def asStaticData = summon[AsStaticData[A]].asStaticData(data)

given staticPrim[A](using prim: Primitive[A]): AsStaticData[A] with
  def asStaticData(a: A): StaticData = Scalar(a)

given staticArray[A](using asStatic: AsStaticData[A], typ: Typable[A]): AsStaticData[Array[A]]
with
  def asStaticData(a: Array[A]): StaticData =
    SArray(typ.identity, a.toSeq.map(asStatic.asStaticData))
