// Adapted from: https://github.com/playframework/play-samples/blob/3.0.x/play-scala-rest-api-example

name := """playapp"""
organization := "com.example"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)
  //.enablePlugins(PlayNettyServer).disablePlugins(PlayPekkoHttpServer) // uncomment to use the Netty backend
  .settings(
    name := "playapp",
    libraryDependencies ++= Seq(
        jdbc,
        "net.logstash.logback" % "logstash-logback-encoder" % "7.3",
        "net.codingwell" %% "scala-guice" % "6.0.0",
        "org.playframework.anorm" %% "anorm" % "2.7.0",
        "org.postgresql" % "postgresql" % "42.5.4"
    )
  )

//crossScalaVersions := Seq("2.13.13", "3.3.3")
// Current 3.5.x release: 3.5.1. Released on September 20, 2024.
crossScalaVersions := Seq("3.5.1")

scalaVersion := crossScalaVersions.value.head

libraryDependencies += guice
