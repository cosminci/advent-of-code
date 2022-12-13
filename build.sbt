name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies += "org.typelevel" %% "cats-core"   % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2"
libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"
libraryDependencies += "dev.optics" %% "monocle-core" % "3.1.0"
libraryDependencies += "dev.optics" %% "monocle-macro" % "3.1.0"

scalacOptions ++= Seq("-deprecation", "-feature")

Global / scalacOptions += "-Ymacro-annotations"
