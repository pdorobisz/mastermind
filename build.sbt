lazy val core = project

lazy val commandline = project.dependsOn(core)
