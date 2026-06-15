package elms.test

import java.net.URLClassLoader
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

import elms.prelude.*
import elms.prelude.given
import elms.core.Type
import elms.codegen.ScalaCodegen
import elms.codegen.Config

abstract class DslDriver[A: Typable, B: Typable]
    extends OptimizingSnippetDriver[A, B](Seq()) with DslOps

trait EvalScalaSnippet[A: Typable, B: Typable] extends SnippetDriver[A, B] {
  val prefix: String
  val name: String

  /** Imports the generated compilation unit needs (e.g. a custom result type
    * that the snippet constructs and names). Rendered as `import <i>` lines. */
  protected def imports: Seq[String] = Seq()

  /** Renderings for types beyond the built-ins, so a driver can teach the
    * codegen how to print a custom `Type` (e.g. `DFAStateT`). */
  protected def extraRenderType: PartialFunction[Type, String] = PartialFunction.empty

  // baseIndentLevel = 2 keeps the body nested inside `object $name { ... }`.
  override val codegen =
    new ScalaCodegen(Config.scalaDefault.copy(baseIndentLevel = 2)) {
      override protected def renderType(ty: Type): String =
        extraRenderType.applyOrElse(ty, (t: Type) => super.renderType(t))
    }

  final case class CompiledDir(outDir: java.nio.file.Path) {
    def loadClass(name: String): Class[?] = {
      val loader =
        new URLClassLoader(
          Array(outDir.toUri.toURL),
          this.getClass.getClassLoader
        )

      loader.loadClass(name)
    }
  }

  private def compileScala(sources: (String, String)*): CompiledDir = {
    val root = Files.createTempDirectory(prefix)
    val srcDir = root.resolve("src")
    val outDir = root.resolve("out")

    Files.createDirectories(srcDir)
    Files.createDirectories(outDir)

    val sourceFiles =
      sources.map { case (name, content) =>
        val file = srcDir.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, content)
        file
      }

    val cp = sys.props("generated.test.classpath")

    val args =
      Seq(
        // generated code may reference @experimental definitions from the test
        // classpath (e.g. types defined under the project's -experimental build)
        "-experimental",
        "-classpath", cp,
        "-d", outDir.toString
      ) ++ sourceFiles.map(_.toString)

    val exit = dotty.tools.dotc.Main.process(args.toArray)

    CompiledDir(outDir)
  }

  override def code = {
    val importLines =
      if imports.isEmpty then "" else imports.map(i => s"import $i\n").mkString
    importLines + s"object $name {\n  " + super.code + "\n}"
  }

  lazy val compiled: Class[?] = compileScala(s"$name.scala" -> code).loadClass(s"$name$$")

  // Look the method up by name: `classOf[Unit]` is `void`, but the compiled
  // parameter is `scala.runtime.BoxedUnit`, so `getMethod` by class would miss.
  private lazy val snippetMethod: java.lang.reflect.Method =
    compiled.getMethods
      .find(_.getName == "snippet")
      .getOrElse(throw new NoSuchMethodException(s"snippet in object $name"))

  def eval(input: A): B = {
    val obj = compiled.getField("MODULE$").get(null)
    snippetMethod.invoke(obj, input.asInstanceOf[AnyRef]).asInstanceOf[B]
  }
}
