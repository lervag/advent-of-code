val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2022",
    version := "0.15.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "1.6.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
