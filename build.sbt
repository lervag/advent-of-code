val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.15.0",
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "3.1.3",
    )
  )
