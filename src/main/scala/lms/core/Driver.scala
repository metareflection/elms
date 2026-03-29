package lms.core

import lms.core.Op
import lms.codegen.ast
import lms.ir

abstract class Driver extends Base {
  protected val builder: ir.Builder
  protected type Exp = builder.Exp
  type Rep[T] = Exp

  def makeFun[A: Typable, B: Typable](
      name: Option[String],
      top: Boolean,
      f: Rep[A] => Rep[B]
  ): Rep[A => B] = {
    val argname = builder.fresh()
    val argty = summon[Typable[A]].identity
    val outty = summon[Typable[B]].identity
    builder.fun(name, top, Vector((argname, argty)), outty) {
      f(unsafeWrap(builder.variable(argname)))
    }
  }

  override def unit[A: Liftable](x: A): Rep[A] = builder.lift(x)
  override def fun[A: Typable, B: Typable](name: Option[String])(
      f: Rep[A] => Rep[B]
  ): Rep[A => B] = makeFun[A, B](name, true, f)
  override def lam[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B] =
    makeFun[A, B](None, false, f)

  override def region[A](exp: => Rep[A]): Rep[A] = unsafeWrap(builder.region(exp))

  override def unsafeWrap[T](exp: Exp): Rep[T] = exp
  override def unsafeUnwrap[T](rep: Rep[T]): Exp = rep
  override def unsafeRegister(op: Op, children: Exp*): Exp = builder
    .reflect(op, children.toVector)

  override def unsafeDeclare[T](name: String): Rep[T] =
    unsafeWrap(builder.variable(builder.name(name)))

  def extract(): ast.Program = builder.extract()
}
