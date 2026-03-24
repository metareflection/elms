package lms.core.macros

/* The main frontend driver for scala3-lms.
 *
 * TODO:
 * - `unRep` is a bad function and should be excised. The main problem is that
 *   `repOrVar` widens the type, ensuring that `1` reports type `Int` instead
 *   of `1`, but `unRep` loses this information. Instead, we should use the
 *   `RepLike` extractor.
 * - Handling of `Var`s in general is very messy and should be rethought.
 * - `dropTrailingUnitInWhileBody` should be rewritten in terms of
 *   `ensureTrailingRep`.
 */

import scala.reflect.ClassTag
import scala.annotation.*
import scala.quoted.*

import lms.util.SourceContext

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
      if (s.isClassDef) then s
      else if (s.isNoSymbol) then Symbol.noSymbol
      else fetchEnclosingClass(s.maybeOwner)

    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

    def makeUnit(x: Term, thist: Term, unitf: Term): Term = {
      val unitt = TypeRepr.of[Unit]
      val unitTyp = Applied(TypeSelect(thist, "Typ"), List(TypeTree.of[Unit]))

      val unitW = Implicits.search(unitTyp.tpe) match {
        case success: ImplicitSearchSuccess => success.tree
      }

      //unit[Unit](())(using Typ.of[Unit])
      Apply(Apply(TypeApply(unitf, List(TypeTree.of[Unit])), List(x)), List(unitW))
    }

    object Visitor extends TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match {
          case Apply(Select(lhsp, "=="), List(rhsp)) => {
            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm

            val lhs = this.transformTerm(lhsp)(owner)
            val rhs = this.transformTerm(rhsp)(owner)

            // TODO
          }
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
