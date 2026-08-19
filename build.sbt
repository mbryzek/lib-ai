name := "lib-ai"

version := "0.1.91"

ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

// The published groupId. Do not remove: nothing else sets it, and without it the
// artifact publishes under a default groupId that no consumer resolves (lib-util 0.0.34).
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

ThisBuild / scalaVersion := "3.8.4"
// Keep the unused browser-automation stack off the test classpath.
//
// It arrives by two transitive routes -- play-test -> io.fluentlenium:fluentlenium-core, and
// scalatestplus-play -> org.seleniumhq.selenium:htmlunit-driver -> net.sourceforge.htmlunit --
// and each of the four packages underneath carries an open high or critical advisory:
// net.sourceforge.htmlunit:htmlunit, org.eclipse.jetty 9.4 (htmlunit's websocket client),
// org.codehaus.plexus:plexus-utils (fluentlenium's maven-model) and io.appium:java-client.
// The worst of them cannot be bumped: net.sourceforge.htmlunit:htmlunit has no release above
// 2.70.0, because the fix shipped under a renamed coordinate (org.htmlunit:htmlunit 3.0.0) that
// nothing on this classpath resolves. Excluding is the only remediation in our hands until
// play-test and htmlunit-driver move to it.
//
// org.seleniumhq.selenium IS DELIBERATELY LEFT ALONE, and htmlunit-driver with it. `play.api.test
// .PlayRunners` -- which `play.api.test.Helpers` extends, and which every GuiceOneServerPerSuite
// spec initializes on its way to starting a test server -- holds `val HTMLUNIT =
// classOf[HtmlUnitDriver]` and `val FIREFOX = classOf[FirefoxDriver]` as class literals, so both
// driver classes must RESOLVE or Helpers' static initializer throws NoClassDefFoundError and
// every server-per-suite spec dies before its first assertion. Resolving them needs only their
// own supertypes, which are all Selenium; neither loads anything from net.sourceforge.htmlunit.
// None of the Selenium artifacts carries an open advisory.
//
// What does go is genuinely unreferenced: the only play-test classes naming fluentlenium are
// TestBrowser and WebDriverFactory, and nothing here constructs a WebDriver, extends a
// scalatestplus-play browser trait, or uses anything from it beyond PlaySpec,
// GuiceOneAppPerSuite and GuiceOneServerPerSuite. A browser test written after this fails to
// link, loudly, at the moment it is written -- which is the intended trade: add a maintained
// stack (org.htmlunit, or Playwright) rather than inherit a stale one through a transitive.
ThisBuild / excludeDependencies ++= Seq(
  ExclusionRule("net.sourceforge.htmlunit"),
  ExclusionRule("io.fluentlenium"),
  ExclusionRule("io.appium")
)

// `-feature` is what makes a `-Werror` failure name the construct, the file and the line.
// Without it the compiler says only "there was 1 feature warning; re-run with -feature",
// and the `ci` log is the only artifact that run leaves -- nobody can re-run it
// interactively. Order below is the two general flags, then the `-W` set alphabetically,
// so a new option has one obvious place to go.
lazy val allScalacOptions = Seq(
  "-feature",
  "-Werror",
  "-Wimplausible-patterns",
  "-Wunused:imports",
  "-Wunused:linted",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wunused:privates",
  // The committed apibuilder client under `app/scala/com/bryzek/ai/generated/` is
  // regenerated, not hand-written, so a warning there is a codegen bug rather than a
  // bug in this repo and `-Werror` must not fail this build for one.
  "-Wconf:src=.*/generated/.*:s"
)

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayScala)
  .settings(
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
