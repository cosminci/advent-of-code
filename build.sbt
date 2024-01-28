name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"      % "2.10.0",
  "org.typelevel" %% "cats-effect"    % "3.5.2",
  "org.typelevel" %% "cats-parse"     % "1.0.0",
  "dev.optics"    %% "monocle-core"   % "3.2.0",
  "dev.optics"    %% "monocle-macro"  % "3.2.0",
  "tools.aqua"     % "z3-turnkey"     % "4.12.2.1",
  "org.jgrapht"    % "jgrapht-core"   % "1.5.2"
)

scalacOptions ++= Seq("-deprecation", "-feature")

Global / scalacOptions += "-Ymacro-annotations"
