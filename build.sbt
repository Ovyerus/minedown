name := "minedown"
version := "0.1"
scalaVersion := "2.13.1"

val circeVersion = "0.12.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"