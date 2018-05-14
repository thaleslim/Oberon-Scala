scalaVersion := "2.12.4"

name := "Mini linguagem de Tecnicas de Programacao"
organization := "br.unb.cic"
version := "1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions ++= Seq("-deprecation")

parallelExecution in Test := false
