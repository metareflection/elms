name := "elms"

ThisBuild / scalaVersion := "3.6.4"

scalacOptions ++= Seq("-experimental")
scalacOptions ++= Seq("-Wconf:msg=match may not be exhaustive:e")
scalacOptions ++= Seq("-language:strictEquality")
scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Test

Test / fork := true

Test / javaOptions += {
  val cp = (Test / fullClasspath).value.files.mkString(java.io.File.pathSeparator)
  s"-Dgenerated.test.classpath=$cp"
}
