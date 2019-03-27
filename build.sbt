import sbt.Keys.mainClass
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val sharedSettings = Seq(
  version in ThisBuild := "0.2",
  scalaVersion := "2.12.6",
  organization in ThisBuild := "ai.dragonfly.code",
  publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" )) ),
  scalacOptions in ThisBuild ++= Seq("-feature"),
  resolvers += "dragonfly.ai" at "http://code.dragonfly.ai/",
  libraryDependencies ++= Seq(
    "ai.dragonfly.code" %%% "snowflake" % "0.2",
    "com.lihaoyi" %% "upickle" % "0.7.1"
  ),
  mainClass in ThisBuild := Some("ai.dragonfly.versionedjson.examples.test.TestVersionedJson")
)

lazy val versionedjson = crossProject(JSPlatform, JVMPlatform).settings(sharedSettings)
