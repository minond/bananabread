val dottyVersion = "3.0.0-RC1"

lazy val root = project
  .in(file("src"))
  .settings(
    name := "sourdough",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalaSource in Test := baseDirectory.value / "test",

    // libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0",
    // libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  )
