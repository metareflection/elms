ThisBuild / scalaVersion := "3.6.4"

scalacOptions ++= Seq("-experimental")

libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
