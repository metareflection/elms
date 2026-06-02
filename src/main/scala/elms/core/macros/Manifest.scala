package elms.core.macros

import elms.core.{Type, Typable, StructManifest, StructRepr}

import scala.quoted.{Expr, Quotes, Type as QType}

private[core] object Manifest {
  def derivedImpl[A: QType](using quotes: Quotes): Expr[StructManifest[A]] = {
    import quotes.reflect.*

    val aTpe = TypeRepr.of[A]
    val sym = aTpe.typeSymbol

    if !sym.flags.is(Flags.Case) then {
      report.errorAndAbort(s"StructManifest can only be derived for case classes; ${aTpe
          .show} is not a case class.")
    }

    val clsName = sym.name

    val memberExprs: List[Expr[(String, Type)]] = sym.caseFields.map { field =>
      {
        val fieldTy = aTpe.memberType(field).dealias
        fieldTy.asType match {
          case '[fieldA] => {
            Expr.summon[Typable[fieldA]] match {
              case Some(typ) => '{ (${ Expr(field.name) }, ${ typ }.identity) }
              case None      => {
                val msg = s"""Cannot derive StructManifest for ${aTpe.show}:
                     |  field `${field.name}: ${fieldTy.show}` does not have a DSL type.
                     |""".stripMargin

                field.pos match {
                  case Some(pos) => report.errorAndAbort(msg, pos)
                  case None      => report.errorAndAbort(msg)
                }
              }
            }
          }
        }
      }
    }

    val membersE: Expr[Seq[(String, Type)]] = Expr.ofSeq(memberExprs)

    '{
      new StructManifest[A] {
        val repr: StructRepr = new StructRepr {
          override val name: String = ${ Expr(clsName) }
          override val members: Map[String, Type] = ($membersE).toMap
        }
      }
    }
  }
}
