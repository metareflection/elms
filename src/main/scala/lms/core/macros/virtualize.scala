package elms.core.macros

/* The main frontend driver for scala3-elms.
 *
 * TODO:
 * - We currently check for a lot of hardcoded names, like `Rep` and some
 *   conversion functions.
 */

import scala.reflect.ClassTag
import scala.annotation.*
import scala.quoted.*

@experimental
class virtualize extends MacroAnnotation {
  override def transform(using
      quotes: Quotes
  )(
      tree: quotes.reflect.Definition,
      companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = {
    import quotes.reflect._

    def findMethods(owner: Symbol, name: String): List[Symbol] =
      if (owner.isNoSymbol) Nil
      else {
        owner.methodMember(name) ++ owner.companionClass.methodMember(name) match {
          case Nil     => findMethods(owner.maybeOwner, name)
          case results => results
        }
      }

    def fetchEnclosingClass(s: Symbol): Symbol =
      if s.isClassDef then s
      else if s.isNoSymbol then Symbol.noSymbol
      else fetchEnclosingClass(s.maybeOwner)

    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

    def makeUnit(x: Term, thist: Term, unitf: Term): Term = {
      val unitt = TypeRepr.of[Unit]
      val liftable = TypeSelect(thist, "Liftable")
      val unitTyp = Applied(liftable, List(TypeTree.of[Unit]))

      val unitW = Implicits.search(unitTyp.tpe) match {
        case success: ImplicitSearchSuccess => success.tree
      }

      // unit[Unit](())(using Typ.of[Unit])
      Apply(Apply(TypeApply(unitf, List(TypeTree.of[Unit])), List(x)), List(unitW))
    }

    def unRep(t: TypeRepr): Option[TypeRepr] = t.widen match {
      case AppliedType(f, List(arg)) =>
        // XXX - do something better
        if f.show.endsWith("Rep") then Some(arg) else None
      case t => None
    }

    def flattenBlockT(t: Statement): (List[Statement], Term) = t match {
      case Block(body, v) => {
        val flattenedBody = body.flatMap(stm => {
          val (stms, endv) = flattenBlockT(stm)
          stms ++ List(endv)
        })
        val (flattenedVStms, trueV) = flattenBlockT(v)
        (flattenedBody ++ flattenedVStms, trueV)
      }
      case t: Term => (Nil, t)
      case stm     => (List(stm), Literal(UnitConstant()))
    }

    def flattenBlock(t: Term): Term = {
      val (body, v) = flattenBlockT(t)
      Block(body, v)
    }

    def ensureTrailingRep(t: Term, thist: Term, unitf: Term): Term = {
      val (body, v) = flattenBlockT(t)
      val inferredTyp = v.tpe

      if unRep(inferredTyp).isDefined then return Block(body, v)

      val ttree = TypeTree.of(using inferredTyp.asType)
      val tLiftable = Applied(TypeSelect(thist, "Liftable"), List(TypeTree.of[Unit]))

      val tLiftableW = Implicits.search(tLiftable.tpe) match {
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure => report.errorAndAbort(
            "couldn't construct type manifest for type Rep[" + inferredTyp.show + "]"
          )
      }

      // unit[T](v)(using Liftable.of[T])
      val repv = Apply(Apply(TypeApply(unitf, List(ttree)), List(v)), List(tLiftableW))

      Block(body, repv)
    }

    object Visitor extends TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case If(Apply(conv, List(x)), thenp, elsep) => {
          val thist = makeThis(owner)

          val unitf = findMethods(owner, "unit") match {
            case Nil => report
                .errorAndAbort("LMS internal error: no [unit] found for self")
            case x :: _ => thist.select(x)
          }

          val xt =
            if (conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
              this.transformTerm(x)(owner)
            } else { return super.transformTerm(tree)(owner) }

          val thent = ensureTrailingRep(this.transformTerm(thenp)(owner), thist, unitf)
          val elset = ensureTrailingRep(this.transformTerm(elsep)(owner), thist, unitf)

          val ttype = thent.tpe.widen

          val trep = unRep(ttype) match {
            case Some(t) => t
            case None    => report.errorAndAbort(
                s"BUG: virtualized if/else body does not have trailing Rep type, instead has ${ttype
                    .show}"
              )
          }

          Select.overloaded(thist, "__ifThenElse", List(trep), List(xt, thent, elset))
        }

        case While(Apply(conv, List(x)), body) => {
          val thist = makeThis(owner)

          val xt =
            if (conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
              this.transformTerm(x)(owner)
            } else { return super.transformTerm(tree)(owner) }

          val unitf = findMethods(owner, "unit") match {
            case Nil => report
                .errorAndAbort("LMS internal error: no [unit] found for self")
            case x :: _ => thist.select(x)
          }

          val bodyt = ensureTrailingRep(this.transformTerm(body)(owner), thist, unitf)

          Select.overloaded(thist, "__whileDo", Nil, List(xt, bodyt))
        }

        case _ => super.transformTerm(tree)(owner)
      }
    }

    tree match {
      case ClassDef(name, constructor, parents, self, body) => {
        val vbody = Visitor.transformStats(body)(tree.symbol.owner)
        val vtree = ClassDef.copy(tree)(name, constructor, parents, self, vbody)
        List(vtree)
      }
      case DefDef(name, params, retTy, Some(rhs)) => {
        val vrhs = Visitor.transformTerm(rhs)(tree.symbol.owner)
        val vtree = DefDef.copy(tree)(name, params, retTy, Some(vrhs))
        List(vtree)
      }
      case _ => {
        report.error("@virtualize must be applied to a top-level class or function")
        List(tree)
      }
    }
  }
}
