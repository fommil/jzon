organization := "dev.zio"
name := "zio-json"

ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.8")
ThisBuild / scalaVersion := crossScalaVersions.value.last

scalacOptions ++= Seq(
  "-language:_",
  "-deprecation"
)

lazy val shapely =
  ProjectRef(uri("https://gitlab.com/fommil/shapely.git#41bba89f12ac01b9d7480e2ff0127a4552c60f3e"), "shapely")
lazy val root = (project in file(".")) dependsOn shapely

Compile / sourceGenerators += Def.task {
  val dir = (Compile / sourceManaged).value
  val gen = List(
    CodeGen.encoders -> dir / "zio" / "json" / "EncoderGenerated.scala",
    CodeGen.decoders -> dir / "zio" / "json" / "DecoderGenerated.scala"
  )
  gen.foreach {
    case (content, file) => IO.write(file, content)
  }
  gen.map(_._2)
}.taskValue

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.2" intransitive ()
libraryDependencies += "eu.timepit" %% "refined"     % "0.9.15" intransitive ()

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "junit"        % "junit"           % "4.11" % Test
)
crossPaths := false // https://github.com/sbt/junit-interface/issues/35
testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
fork := true

// TODO prove that it works on Scala 3

//////////////////////////////
// PERF TESTING
enablePlugins(NeoJmhPlugin)
inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)

libraryDependencies ++= Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.5.0"  % "test,jmh",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.5.0"  % "test,jmh",
  "io.circe"                              %% "circe-generic-extras"  % "0.13.0" % "test,jmh",
  "com.typesafe.play"                     %% "play-json"             % "2.9.0"  % "test,jmh",
  "ai.x"                                  %% "play-json-extensions"  % "0.42.0" % "test,jmh"
)
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.13.0" % "test,jmh")
//////////////////////////////

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt jmh:scalafmt")
