ThisBuild / scalaVersion := "3.3.0"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

//enablePlugins(ScalaNativePlugin)

// import to add Scala Native options
//import scala.scalanative.build._

// defaults set with common options shown
//nativeConfig ~= { c =>
//  c.withLTO(LTO.thin)
//  //c.withLTO(LTO.none)
//    .withMode(Mode.release)
//    .withGC(GC.none)
//}

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.15.0",
    semanticdbEnabled := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "3.1.3",
    )
  )
