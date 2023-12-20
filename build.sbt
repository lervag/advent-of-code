ThisBuild / scalaVersion := "3.3.0"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

// Comment this out to use Scala Native
// enablePlugins(ScalaNativePlugin)
// import scala.scalanative.build._
// nativeConfig ~= { c =>
//   c
//     .withLTO(LTO.thin)
//     .withMode(Mode.release)
//     .withGC(GC.none)
// }

// For å kunne lage en native app
enablePlugins(NativeImagePlugin)


// For å teste ytelse på kald JVM:
// fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.15.0",
    semanticdbEnabled := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "3.1.3"
    ),
    nativeImageOptions += "--no-fallback",
    nativeImageVersion := "22.1.0"
  )
