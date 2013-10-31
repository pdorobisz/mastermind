import AssemblyKeys._

version := "1.0"

lazy val buildSettings = Seq(
  version := "1.0",
  organization := "pdorobisz",
  scalaVersion := "2.10.2"
)

// subprojects

lazy val core = project

val commandline = project
  .settings(buildSettings: _*)
  .settings(assemblySettings: _*)
  .settings(
    outputPath in assembly := target.value / "mastermind.jar"
  )
  .dependsOn(core)
