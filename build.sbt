ThisBuild / scalaVersion := "3.6.4"

scalacOptions ++= Seq("-experimental")

libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
