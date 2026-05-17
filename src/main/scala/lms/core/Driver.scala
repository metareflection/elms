package lms.core

import scala.collection.mutable

import lms.core.{Op, Name}
import lms.core.tree as ast
import lms.pipeline
import lms.util.ClosureCompare

abstract class Driver extends Base with ClosureCompare {
  protected val builder: pipeline.Builder
  protected type Exp = builder.Exp
  case class Rep[+T](wrapped: Exp)

  def variable[A](name: Name): Rep[A] = unsafeWrap(builder.variable(name))

  val funTable: mutable.Map[String, Exp] = mutable.Map()

  def makeFun[A: Typable, B: Typable](
      name: Name,
      top: Boolean,
      f: Rep[A] => Rep[B]
  ): Rep[A => B] = {
    val key = canonicalize(f.asInstanceOf[Serializable])
    funTable.get(key) match {
      case Some(symb) => unsafeWrap(symb)
      case None       => {
        val argname = builder.fresh()
        val argty = summon[Typable[A]].identity
        val outty = summon[Typable[B]].identity

        val stub = builder.fun(name, top, Vector((argname, argty)), outty)
        funTable(key) = stub.symbol

        stub.fill { unsafeUnwrap(f(variable(argname))) }

        unsafeWrap(stub.symbol)
      }
    }
  }

  override def fun[A: Typable, B: Typable](name: Option[Name])(
      f: Rep[A] => Rep[B]
  ): Rep[A => B] = makeFun[A, B](name.getOrElse { builder.fresh() }, true, f)
  override def lam[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B] =
    makeFun[A, B](builder.fresh(), false, f)

  override def region[A](exp: => Rep[A]): Rep[A] =
    unsafeWrap(builder.region { unsafeUnwrap(exp) })

  override def staticData[A: AsStaticData](data: A): Rep[A] =
    unsafeWrap(builder.registerStaticData(data.asStaticData))

  override def unsafeWrap[T](exp: Exp): Rep[T] = Rep(exp)
  override def unsafeUnwrap[T](rep: Rep[T]): Exp = rep.wrapped
  override def unsafeRegister(op: Op, children: Exp*): Exp = builder
    .reflect(op, children.toVector)

  override def unsafeDeclare[T](name: String): Rep[T] = variable(builder.name(name))

  override def unsafeWithFresh[A, B](f: (Name, Rep[A]) => Rep[B]): Rep[B] = {
    val name = builder.fresh()
    val v = variable(name)
    f(name, v)
  }

  def extract(): ast.Program = builder.extract()
}
