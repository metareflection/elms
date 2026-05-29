package elms.test

import java.net.URLClassLoader
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

import elms.prelude.*
import elms.prelude.given
import elms.codegen.ScalaCodegen
import elms.codegen.Config

abstract class DslDriver[A: Typable, B: Typable]
    extends OptimizingSnippetDriver[A, B](Seq()) with DslOps

trait EvalScalaSnippet[A: Typable, B: Typable] extends SnippetDriver[A, B] {
  override val codegen = ScalaCodegen(Config.scalaDefault.copy(baseIndentLevel = 2))

  val prefix: String
  val name: String

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
        "-classpath", cp,
        "-d", outDir.toString
      ) ++ sourceFiles.map(_.toString)

    val exit = dotty.tools.dotc.Main.process(args.toArray)

    CompiledDir(outDir)
  }

  override def code = s"object $name {\n  " + super.code + "\n}"

  lazy val compiled: Class[?] = compileScala(s"$name.scala" -> code).loadClass(s"$name$$")

  def eval(cls: Class[A], input: A): B = {
    val snippetF = compiled.getMethod("snippet", cls)
    val obj = compiled.getField("MODULE$").get(null)
    snippetF.invoke(obj, input).asInstanceOf[B]
  }
}
