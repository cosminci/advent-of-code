name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.12"

libraryDependencies += "org.typelevel" %% "cats-core"   % "2.10.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2"
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0"
libraryDependencies += "dev.optics" %% "monocle-core" % "3.2.0"
libraryDependencies += "dev.optics" %% "monocle-macro" % "3.2.0"

scalacOptions ++= Seq("-deprecation", "-feature")

Global / scalacOptions += "-Ymacro-annotations"
