name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies += "org.typelevel" %% "cats-core"   % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2"

scalacOptions ++= Seq("-deprecation", "-feature")
