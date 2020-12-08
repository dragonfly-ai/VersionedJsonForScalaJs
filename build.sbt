ThisBuild / scalaVersion := "2.13.3"

lazy val root = project.in(file(".")).
  aggregate(versionedjson.js, versionedjson.jvm).
  settings(
    publishTo := Some( Resolver.file("file",  new File( "/var/www/maven" ) ) ),
  )

lazy val versionedjson = crossProject(JSPlatform, JVMPlatform).
  settings(
    publishTo := Some(Resolver.file("file",  new File("/var/www/maven"))),
    name := "versionedjson",
    version := "0.2",
    resolvers ++= Seq(
      "code.dragonfly.ai" at "https://code.dragonfly.ai"
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" % "ujson_2.13" % "1.2.2"
    ),
    organization := "ai.dragonfly.code",
    mainClass in (Compile, run) := Some("ai.dragonfly.versionedjson.examples.Test")
  ).
  jvmSettings(
    // Add JVM-specific settings here
    //mainClass in (Compile, run) := Some("ai.dragonfly.versionedjson.examples.Test")
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided",
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "com.lihaoyi" % "ujson_sjs1_2.13" % "1.2.2"
    ),
    scalaJSUseMainModuleInitializer := true
  )

/*
import sbt.Keys.mainClass
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val sharedSettings = Seq(
  version in ThisBuild := "0.1",
  scalaVersion := "2.12.6",
  organization in ThisBuild := "ai.dragonfly.code",
  publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  scalacOptions in ThisBuild ++= Seq("-feature"),
  resolvers += "dragonfly.ai" at "http://code.dragonfly.ai:8080/",
  libraryDependencies ++= Seq(
    "ai.dragonfly.code" %%% "snowflake" % "0.2",
    "com.lihaoyi" %%% "upickle" % "0.7.1"
  ),
  mainClass in ThisBuild := Some("ai.dragonfly.versionedjson.examples.test.TestVersionedJson")
)

lazy val versionedjson = crossProject(JSPlatform, JVMPlatform).settings(sharedSettings)
*/
