val testLibraries = List(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "discipline" % "0.7.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")

val catsLibraries = List(
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.typelevel" %% "cats" % "0.8.1")

val simulacrumLibrary = List(
  "com.github.mpilquist" %% "simulacrum" % "0.10.0")

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "com.alexknvl",
  version := "0.1.0",
  scalaVersion := "2.12.1",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-Yno-adapted-args", "-Ywarn-dead-code",
    "-Ywarn-numeric-widen", "-Xfuture"),
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),
  libraryDependencies ++= testLibraries,
  wartremoverWarnings ++= Warts.all
)

val macroParadiseV = "2.0.1"

lazy val root = (project in file("."))
  .settings(name := "taseq")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= catsLibraries)
  .settings(libraryDependencies ++= List(
    "com.alexknvl" %% "leibniz" % "0.2.0"
  ))
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % macroParadiseV cross CrossVersion.full),
          "org.scalamacros" %% "quasiquotes" % macroParadiseV cross CrossVersion.binary
        )
    })
  )
