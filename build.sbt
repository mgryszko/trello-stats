organization := "com.grysz"

name := "trello-stats"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.0",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

