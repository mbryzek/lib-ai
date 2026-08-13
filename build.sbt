name := "lib-ai"

version := "0.1.83"

ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

ThisBuild / organization := "com.bryzek"
ThisBuild / homepage := Some(url("https://github.com/mbryzek/lib-ai"))
ThisBuild / licenses := Seq("MIT" -> url("https://github.com/mbryzek/lib-ai/blob/main/LICENSE"))
ThisBuild / developers := List(
  Developer("mbryzek", "Michael Bryzek", "mbryzek@alum.mit.edu", url("https://github.com/mbryzek"))
)
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/mbryzek/lib-ai"), "scm:git@github.com:mbryzek/lib-ai.git")
)

ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / sonatypeRepository := "https://central.sonatype.com/api/v1/publisher"
ThisBuild / publishMavenStyle := true

ThisBuild / scalaVersion := "3.8.4"

lazy val allScalacOptions = Seq(
  "-Werror",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wimplausible-patterns",
  "-Wunused:imports",
  "-Wunused:privates",
  "-Wunused:linted",
  "-Wconf:src=.*/generated/.*:s"
)

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayScala)
  .settings(
    resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
    scalafmtOnCompile := true,
    Compile / packageDoc / mappings := Seq(),
    Compile / packageDoc / publishArtifact := true,
    // ISS-356: `-oDF` prints a per-test duration to stdout; `-u` writes one JUnit XML file per
    // suite, which is the only correct per-suite attribution this build produces. stdout carries
    // no per-line suite marker, so once suites run in parallel a reader (devops/bin/test-timings)
    // can only attribute a duration to whichever "[info] ClassName:" header was printed most
    // recently by ANY thread -- which on a real run named the wrong suite for every test it
    // reported as slow.
    testOptions ++= Seq(
      Tests.Argument("-oDF"),
      Tests.Argument(TestFrameworks.ScalaTest, "-u", (target.value / "test-reports").getAbsolutePath)
    ),
    // ISS-2173: the Play plugin sets `Test / fork := true`, and a forked JVM inherits none of
    // `SBT_OPTS` -- left unset its max heap is a QUARTER OF PHYSICAL RAM, which is 16G on the 64G
    // laptop and 6G on the 24G mini. Without this pin the CI heap measurement behind
    // `ci/build.sh`'s `# ci-needs: heap:4G` would be a fact about whichever machine happened to run
    // it rather than about this suite. platform and acumen pin theirs for the same reason.
    Test / javaOptions += "-Xmx4g",
    scalacOptions ++= allScalacOptions,
    libraryDependencies ++= Seq(
      ws,
      "joda-time" % "joda-time" % "2.14.3",
      "com.google.inject" % "guice" % "5.1.0",
      "org.playframework" %% "play-json" % "3.0.6",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test
    )
  )
