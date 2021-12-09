ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "day6"
  )

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "2.0.1-RC1",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "2.0.1-RC1"
)
