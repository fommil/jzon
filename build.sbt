organization := "com.fommil"
name := "jzon"

ThisBuild / crossScalaVersions := Seq("2.13.10", "2.12.17", "3.2.1")
ThisBuild / scalaVersion := crossScalaVersions.value.head

scalacOptions ++= Seq(
  "-language:_",
  "-deprecation"
)

Compile / sourceGenerators += Def.task {
  val dir = (Compile / sourceManaged).value
  val gen = List(
    CodeGen.encoders -> dir / "jzon" / "EncoderGenerated.scala",
    CodeGen.decoders -> dir / "jzon" / "DecoderGenerated.scala"
  )
  gen.foreach {
    case (content, file) => IO.write(file, content)
  }
  gen.map(_._2)
}.taskValue

libraryDependencies ++= Seq(
  "com.fommil"   %% "shapely"        % "1.0.0",
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "junit"        % "junit"           % "4.13.2" % Test
)
Test / crossPaths := false // https://github.com/sbt/junit-interface/issues/35
testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
fork := true

//////////////////////////////
// PERF TESTING
enablePlugins(NeoJmhPlugin)
inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)

libraryDependencies ++= {
  // circe-generic-extras and play-json-extensions not available for Scala 3
  if (scalaVersion.value.startsWith("3")) Nil
  else
    Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.12.1" % "test,jmh",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.12.1" % "test,jmh",
      "io.circe"                              %% "circe-generic-extras"  % "0.13.0" % "test,jmh",
      "io.circe"                              %% "circe-fs2"             % "0.14.0" % "test,jmh",
      "co.fs2"                                %% "fs2-io"                % "3.2.4"  % "test,jmh",
      "com.typesafe.play"                     %% "play-json"             % "2.9.0"  % "test,jmh",
      "ai.x"                                  %% "play-json-extensions"  % "0.42.0" % "test,jmh"
    ) ++ Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.14.1" % "test,jmh")
} //////////////////////////////

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt jmh:scalafmt")

// +publishSigned ; sonatypeBundleRelease

ThisBuild / licenses := List("BSD-2" -> url("https://opensource.org/licenses/BSD-2-Clause"))
Compile / resources += baseDirectory.value / "LICENSE"
ThisBuild / publishTo := sonatypePublishToBundle.value

homepage := Some(url("https://github.com/fommil/jzon"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/fommil/jzon"),
    "scm:git@github.com:fommil/jzon.git"
  )
)
developers := List(
  Developer(id="fommil", name="Sam Halliday", email="sam.halliday@gmail.com", url=url("https://fommil.com"))
)
