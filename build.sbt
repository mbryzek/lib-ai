name := "lib-ai"

organization := "com.mbryzek"

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Wunused:locals",
  //"-Wunused:params",
  "-Wimplausible-patterns",
  //"-Wunused:linted",
  "-Wunused:unsafe-warn-patvars",
  //"-Wunused:imports",
  "-Wunused:privates",
)

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayScala)
  .settings(
    resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
    scalafmtOnCompile := true,
    Compile / doc / sources := Seq.empty,
    Compile / packageDoc / publishArtifact := false,
    testOptions += Tests.Argument("-oDF"),
    scalacOptions ++= allScalacOptions,
    libraryDependencies ++= Seq(
      ws,
      "joda-time" % "joda-time" % "2.14.0",
      "com.google.inject" % "guice" % "5.1.0",
      "org.playframework" %% "play-json" % "3.0.5",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
    ),
  )
