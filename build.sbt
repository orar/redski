import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.redmart",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "redmart_ski",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "com.typesafe.akka" %% "akka-stream" % "2.5.14"
    )
  )
TaskKey[Unit]("runski") := (runMain in Compile).toTask(" com.example.Hello").value