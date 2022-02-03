organization := "dev.zio"
name := "zio-json"

ThisBuild / crossScalaVersions := Seq("2.13.8", "2.12.15", "3.1.1")
ThisBuild / scalaVersion := crossScalaVersions.value.head

scalacOptions ++= Seq(
  "-language:_",
  "-deprecation"
)

lazy val shapely =
  ProjectRef(uri("https://gitlab.com/fommil/shapely.git#f4135d96faf9cfa8d63d6fd7706666eb6243556f"), "shapely")
lazy val root = (project in file("."))
  .dependsOn(shapely)
  .settings(
    // https://www.yourkit.com/docs/java/help/startup_options.jsp
    // Jmh / neoJmhYourkit := Seq("disabletracing,disablenatives,exceptions=disable,allocsampled")
  )

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

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.6" intransitive ()
libraryDependencies += "eu.timepit" %% "refined"     % "0.9.28" intransitive ()

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11"   % Test,
  "junit"        % "junit"           % "4.13.2" % Test
)
crossPaths := false // https://github.com/sbt/junit-interface/issues/35
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
