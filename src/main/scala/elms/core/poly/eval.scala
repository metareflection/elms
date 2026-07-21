package elms.core.poly.eval

import elms.core.__Virtualized
import elms.core.{Primitive, Typable, INT, BOOL, CHAR, UNIT, STRING}
import elms.core.poly._

class Driver
    extends HasRep
    with PrimitiveOps
    with BooleanOps
    with EqualityOps
    with IntegerOps
    with RangeOps
    with StringOps
    with ArrayOps {
  case class Rep[+T](v: T) extends __Virtualized[T]

  // Lift
  given liftablePrimitive[A: Primitive](using Typable[A]): Liftable[A] with
    def lift(x: A): Rep[A] = Rep(x)

  // PrimitiveOps
  def __ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] =
    if (c.v) t else e

  def __whileDo(guard: => Rep[Boolean], body: => Rep[Unit]): Rep[Unit] = {
    while (guard.v) { body }
    Rep(())
  }

  extension [B](f: Rep[() => B]) def apply(): Rep[B] = Rep(f.v())

  given repApply1Fun[A, B]: RepApply1[A => B, A, B] with
    def run(f: Rep[A => B], arg: Rep[A]): Rep[B] = Rep(f.v(arg.v))

  extension [A1, A2, B](f: Rep[(A1, A2) => B])
    def apply(a1: Rep[A1], a2: Rep[A2]): Rep[B] = Rep(f.v(a1.v, a2.v))

  extension [A1, A2, A3, B](f: Rep[(A1, A2, A3) => B])
    def apply(a1: Rep[A1], a2: Rep[A2], a3: Rep[A3]): Rep[B] =
      Rep(f.v(a1.v, a2.v, a3.v))

  // BooleanOps
  extension (lhs: Rep[Boolean])
    def &&(rhs: => Rep[Boolean]): Rep[Boolean] = Rep(lhs.v && rhs.v)
    def ||(rhs: => Rep[Boolean]): Rep[Boolean] = Rep(lhs.v || rhs.v)
    def unary_! : Rep[Boolean] = Rep(!lhs.v)

  // EqualityOps
  extension [T](lhs: Rep[T])(using CanEqual[T, T])
    def ===(rhs: Rep[T]): Rep[Boolean] = Rep(lhs.v == rhs.v)

  // IntegerOps
  extension (lhs: Rep[Int])
    def +(rhs: Rep[Int]): Rep[Int] = Rep(lhs.v + rhs.v)
    def -(rhs: Rep[Int]): Rep[Int] = Rep(lhs.v - rhs.v)
    def *(rhs: Rep[Int]): Rep[Int] = Rep(lhs.v * rhs.v)
    def <(rhs: Rep[Int]): Rep[Boolean] = Rep(lhs.v < rhs.v)
    def >(rhs: Rep[Int]): Rep[Boolean] = Rep(lhs.v > rhs.v)
    def <=(rhs: Rep[Int]): Rep[Boolean] = Rep(lhs.v <= rhs.v)
    def >=(rhs: Rep[Int]): Rep[Boolean] = Rep(lhs.v >= rhs.v)

  // RangeOps
  extension (st: Rep[Int]) def until(end: Rep[Int]): Rep[Range] = Rep(st.v.until(end.v))

  extension (lhs: Rep[Range])
    def start: Rep[Int] = Rep(lhs.v.start)
    def end: Rep[Int] = Rep(lhs.v.end)
    def foreach(body: Rep[Int] => Rep[Unit]): Unit = lhs.v.foreach(i => body(Rep(i)))

  // StringOps
  extension (s: Rep[String])
    def take(x: Rep[Int]): Rep[String] = Rep(s.v.take(x.v))
    def drop(x: Rep[Int]): Rep[String] = Rep(s.v.drop(x.v))
    def startsWith(haystack: Rep[String]): Rep[Boolean] = Rep(s.v.startsWith(haystack.v))
    def endsWith(haystack: Rep[String]): Rep[Boolean] = Rep(s.v.endsWith(haystack.v))
    def charAt(i: Rep[Int]): Rep[Char] = Rep(s.v.charAt(i.v))
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String] = Rep(s.v.substring(st.v, end.v))

  given stringLength: RepLength[String] with
    def run(x: Rep[String]): Rep[Int] = Rep(x.v.length)

  // ArrayOps
  def newArray[A: Typable](i: Rep[Int]): Rep[Array[A]] = {
    val n = i.v
    val arr = summon[Typable[A]].identity match {
      case INT    => new Array[Int](n)
      case BOOL   => new Array[Boolean](n)
      case CHAR   => new Array[Char](n)
      case UNIT   => new Array[Unit](n)
      case STRING => new Array[String](n)
      case _      => new Array[AnyRef](n)
    }
    Rep(arr.asInstanceOf[Array[A]])
  }

  def initFrom[A: Typable](entries: Seq[A]): Rep[Array[A]] = {
    val arr = newArray[A](Rep(entries.length))
    entries.zipWithIndex.foreach { case (x, i) => arr.set(Rep(i), Rep(x)) }
    arr
  }

  extension [A](arr: Rep[Array[A]])
    def get(i: Rep[Int]): Rep[A] = Rep(arr.v(i.v))
    def set(i: Rep[Int], x: Rep[A]): Rep[Unit] = Rep(arr.v(i.v) = x.v)

  given arrayLength[A]: RepLength[Array[A]] with
    def run(arr: Rep[Array[A]]): Rep[Int] = Rep(arr.v.length)
}
