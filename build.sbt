val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "day-1",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "1.6.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
