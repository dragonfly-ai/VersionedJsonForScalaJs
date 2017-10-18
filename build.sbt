import sbt.Keys.{scalacOptions, _}

scalaVersion in ThisBuild := "2.12.3"

name in ThisBuild := "versionedjson"

organization in ThisBuild := "ai.dragonfly.code"

version in ThisBuild := "0.1"

publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" )) )

val versionedjson = crossProject.settings(
  // shared settings
  libraryDependencies ++= Seq(
    "com.github.benhutchison" %%% "microjson" % "1.4",
    "org.scala-lang" % "scala-reflect" % "2.12.3"
  )
).jsSettings(
  // JS-specific settings here
  jsDependencies += RuntimeDOM
).jvmSettings(
  // JVM-specific settings here
  libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
)

lazy val js = versionedjson.js

lazy val jvm = versionedjson.jvm

scalacOptions += "-feature"