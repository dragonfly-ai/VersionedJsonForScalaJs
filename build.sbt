import sbt.Keys._

scalaVersion in ThisBuild := "2.12.0"

name in ThisBuild := "distributed"

organization in ThisBuild := "ai.dragonfly"

version in ThisBuild := "0.1"

publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" )) )

val datatypes = crossProject.settings(
  // shared settings
  libraryDependencies ++= Seq("com.github.benhutchison" %%% "microjson" % "1.4")
).jsSettings(
  // JS-specific settings here
  jsDependencies += RuntimeDOM
).jvmSettings(
  // JVM-specific settings here
  libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
)

lazy val js = datatypes.js

lazy val jvm = datatypes.jvm

mainClass in (Compile,run) := Some("ai.dragonfly.types.TestVersionedJSON")

