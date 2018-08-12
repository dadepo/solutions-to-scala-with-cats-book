
lazy val root = (project in file(".")).
  enablePlugins(JavaAppPackaging).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "book exercises",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.typelevel" %% "cats-core" % "1.1.0",
      "org.typelevel" %% "cats-core" % "1.1.0" % Test
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-feature",
      "-Xfatal-warnings",
      "-Ypartial-unification",
      "-language:implicitConversions"
      )
  )

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

resolvers += Resolver.sonatypeRepo("snapshots")

lazy val core = (project in file("core"))
  .settings(name := "Hello Core")

