ThisBuild / scalaVersion := "2.13.3"

lazy val root = project.in(file(".")).
  aggregate(versionedjson.js, versionedjson.jvm).
  settings(
    publishTo := Some( Resolver.file("file",  new File( "/var/www/maven" ) ) )
  )

lazy val versionedjson = crossProject(JSPlatform, JVMPlatform).
  settings(
    name := "versionedjson",
    version := "0.203",
    organization := "ai.dragonfly.code",
    resolvers += "code.dragonfly.ai" at "https://code.dragonfly.ai:4343",
    libraryDependencies ++= Seq(
      "com.lihaoyi" % "ujson_2.13" % "1.2.3"
    ),
    scalacOptions ++= Seq("-feature", "-deprecation"),
    mainClass in (Compile, run) := Some("ai.dragonfly.versionedjson.examples.Test"),
    publishTo := Some(Resolver.file("file",  new File("/var/www/maven")))
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.0.0",
      "org.scala-lang" % "scala-reflect" % "2.13.3"
    )
  ).
  jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" % "ujson_sjs1_2.13" % "1.2.3"
    ),
    scalaJSUseMainModuleInitializer := true
  )
