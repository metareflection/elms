package elms.test

import scala.language.implicitConversions

import elms.prelude.{_, given}
import elms.helpers.OptimizingSnippetDriver
import elms.helpers.DslOps
import elms.core.StructManifest
import elms.codegen.CCodegen

@virtualize
class StructTests extends SnapshotFunSuite {
  val under = "cstruct/"

  abstract class DslDriverC[A: Typable, B: Typable]
      extends OptimizingSnippetDriver[A, B](Seq()) with DslOps {
    override val codegen = CCodegen()
  }

  case class Foo(x: Int, y: String) derives StructManifest

  test("get") {
    object Snippet extends DslDriverC[Foo, Int] with DslOps {
      def snippet(s: Rep[Foo]): Rep[Int] = {
        s.get("x").asInstanceOf[Rep[Int]]
      }
    }
    check("get", Snippet.code)
  }

  test("set") {
    object Snippet extends DslDriverC[Foo, Unit] with DslOps {
      def snippet(s: Rep[Foo]): Rep[Unit] = {
        s.set("x", 6)
      }
    }
    check("set", Snippet.code)
  }
}
