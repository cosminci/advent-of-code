name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.15"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"      % "2.12.0",
  "org.typelevel" %% "cats-effect"    % "3.5.4",
  "org.typelevel" %% "cats-parse"     % "1.0.0",
  "dev.optics"    %% "monocle-core"   % "3.2.0",
  "dev.optics"    %% "monocle-macro"  % "3.2.0",
  "tools.aqua"     % "z3-turnkey"     % "4.13.0",
  "org.jgrapht"    % "jgrapht-core"   % "1.5.2"
)

scalacOptions ++= Seq("-deprecation", "-feature")

Global / scalacOptions += "-Ymacro-annotations"
