package lms.test

import java.io._
import org.scalatest.funsuite.AnyFunSuite

trait SnapshotFunSuite extends AnyFunSuite {
  // global override to accept all diffs
  val overwriteCheckFiles = false

  val prefix = "src/out/"
  val under: String

  def readFile(name: String): String = {
    try {
      val buf = new Array[Byte](new File(name).length().toInt)
      val fis = new java.io.FileInputStream(name)
      fis.read(buf)
      fis.close()
      new String(buf)
    } catch { case e: IOException => "" }
  }

  def writeFile(name: String, content: String) = {
    val out = new java.io.PrintWriter(new File(name))
    out.write(content)
    out.close()
  }

  def check(
      label: String,
      actual: String,
      ext: String = "scala",
      accept: Boolean = false
  ) = {
    val filePrefix = prefix + under + label
    val checkName = filePrefix + ".check." + ext
    val actualName = filePrefix + ".actual." + ext

    val expected = readFile(checkName)

    if expected != actual then {
      if accept || overwriteCheckFiles then {
        println(s"overwriting snapshot at `$checkName`")
        writeFile(checkName, actual)
      } else {
        writeFile(actualName, actual)
        assert(expected == actual)
      }
    }
  }
}
