ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2024"
  )
