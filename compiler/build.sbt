val dottyVersion = "3.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sourdough",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.4" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
  )
