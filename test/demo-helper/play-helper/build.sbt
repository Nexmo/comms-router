name := """play-helper"""
organization := "com.softavail.comms-router.helper"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.4"
scalaVersion in ThisBuild := "2.12.4"
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum"     % "0.12.0",
  "com.chuusai"          %% "shapeless"      % "2.3.3" ,
  "org.scalaz"           %% "scalaz-core"    % "7.2.20"
)
libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.softavail.comms-router.helper.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.softavail.comms-router.helper.binders._"
