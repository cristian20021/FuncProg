
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "FuncProg",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M9", // HTTP client for Scala 3
      "com.softwaremill.sttp.client4" %% "circe" % "4.0.0-M9", // JSON support for sttp
      "io.circe" %% "circe-generic" % "0.14.6", // JSON parsing
      "com.softwaremill.sttp.client4" %% "async-http-client-backend" % "4.0.0-M9" // Async HTTP client backend
    )
  )