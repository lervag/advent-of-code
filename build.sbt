ThisBuild / scalaVersion := "3.6.2"
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
// enablePlugins(NativeImagePlugin)

// For å teste ytelse på kald JVM:
// fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.15.0",
    semanticdbEnabled := true,
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ykind-projector",
      "-Wunused:implicits",
      "-Wunused:explicits",
      "-Wunused:imports",
      "-Wunused:params",
      "-Wunused:privates",
      // "-Xfatal-warnings",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.0.2",
      "org.typelevel" %% "cats-parse" % "1.1.0",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.scalanlp" %% "breeze-viz" % "2.1.0"
    )
    // nativeImageOptions += "--no-fallback",
    // nativeImageVersion := "22.1.0"
  )
