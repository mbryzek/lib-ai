name := "lib-ai"

version := "0.1.94"

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

// Every Jackson artifact in this build resolves to one version.
//
// Jackson's own compatibility rule is that a release train moves together: the datatype and
// dataformat modules compile against databind's internal serializer/deserializer SPI, and
// jackson-module-scala additionally asserts its databind version at runtime and refuses to
// register outside its own minor line. Only that last one fails loudly; a datatype module left
// behind on an older line links fine and throws AbstractMethodError or NoSuchMethodError on
// whichever serializer path first touches a changed SPI method. `JacksonPinSpec` asserts the pair
// registers, so a partial bump fails by name here rather than opaquely in a consumer.
//
// Drift is the default here rather than an accident. Jackson reaches this build only transitively,
// by two routes -- play-json, and Play itself through pekko-serialization-jackson -- and neither is
// a coordinate this build names, so there is no version here to bump: play-json has no release
// above 3.0.6 and the Play version comes from the sbt plugin. Pinning a single coordinate wins the
// conflict only for that artifact and the ones it depends on, leaving cbor/jdk8/jsr310/
// parameter-names behind. Overriding the whole family is what makes one version true of all of
// them.
//
// The floor is a security one and two advisories set it, so it is stated as a range rather than a
// single number. jackson-core below 2.15.0 has no nesting-depth limit and throws StackOverflowError
// on deeply nested input rather than rejecting it (GHSA-h46c-h94j-95f3), and that is the version
// play-json resolves. jackson-databind below 2.18.8 -- and again on 2.19.0 through 2.21.3 --
// validates a type id carrying generics by the substring before the `<` and then resolves the type
// arguments out of the rest of it without ever offering them to the PolymorphicTypeValidator, so
// an allow-list naming one safe container admits any type smuggled into that container's parameter
// position (GHSA-j3rv-43j4-c7qm).
//
// The second is the binding one, and it is why the first is not the number to read off this
// comment: it rules out the whole 2.15.0-2.18.7 span the nesting-depth floor would allow, so the
// lowest this pin may state is 2.18.8, and anything chosen on the 2.19 line must be 2.21.4 or
// above. 2.22.2 is the head of the Jackson 2 line and the version platform and acumen pin, so a
// consumer that pins too resolves one Jackson rather than two.
//
// This governs THIS build's resolution only -- sbt writes no `dependencyOverrides` into the
// published POM -- so it decides what this repo compiles and tests against and imposes no floor on
// a consumer. A consumer states its own, as platform and acumen do.
//
// jackson-annotations publishes no patch versions on its 2.20+ lines (maven-metadata.xml runs
// 2.19.4, 2.20, 2.21, 2.22), so it carries its own version and a patch number there is a 404 that
// fails the whole resolution.
lazy val jacksonVersion = "2.22.2"
lazy val jacksonAnnotationsVersion = "2.22"

ThisBuild / dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
  "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
  "com.fasterxml.jackson.core" % "jackson-annotations" % jacksonAnnotationsVersion,
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor" % jacksonVersion,
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % jacksonVersion,
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % jacksonVersion,
  "com.fasterxml.jackson.module" % "jackson-module-parameter-names" % jacksonVersion,
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion,
)

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
//
// `-Wvalue-discard` (E175) and `-Wnonunit-statement` (E176) are here for ONE class of silent
// bug: a computation that produced a value carrying its own outcome -- a `Future` nobody
// awaited, an `Either`/`Try`/`ValidatedNec` nobody inspected -- thrown away, so the code
// compiles, runs and reports success while the work it stands for never happened. Fixing one
// means HANDLING the value: await it, fold it, bind it, return it. Adapting it to `Unit`
// re-hides the exact thing the flag found, so it is not a fix.
//
// RAISED ON THE VALUE'S TYPE, NOT ON EVERY STATEMENT, and the three `-Wconf` lines below are
// how. Bare, the pair fires on every statement whose value goes unused: a `mutable.Map#put`
// returning the entry it displaced, a Java builder returning itself, a generated DAO `insert`
// returning the row it just wrote, a ScalaTest matcher returning the `Assertion` it has already
// thrown on. None of those is a dropped result, and none of them has any remedy other than the
// `: Unit` ascription this rule forbids -- so the two ids are silenced, then re-raised for the
// types where a discard IS the defect. Re-raised as an ERROR rather than a warning, so the gate
// does not depend on `-Werror` staying on above it.
//
// ORDER IS LOAD-BEARING: a later `-Wconf` rule beats an earlier one, so the sequence is
// silence-then-raise and never the reverse. Growing the gate means adding a type to the last
// line; deleting the two silencing lines is not the same thing and never was -- it surfaces
// tens of thousands of statements whose only legal fix this rule has ruled out.
//
// `-Xlint` IS NOT IN THIS SET, and its Scala 3 replacement is why. The option is Scala 2's;
// Scala 3 deprecates it and schedules it for removal, pointing at `-Wshadow` instead -- and
// `-Wshadow` fires on a constructor parameter forwarded to a base class that re-exposes it as a
// `val` of the same name, which is the only spelling that relationship has. Its remedy is a
// worse parameter name at every named-argument call site, so the lint costs more than the
// shadowing it reports.
lazy val allScalacOptions = Seq(
  "-feature",
  "-Werror",
  "-Wimplausible-patterns",
  "-Wnonunit-statement",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wconf:id=E175:s",
  "-Wconf:id=E176:s",
  "-Wconf:msg=value of type ((scala\\.concurrent\\.)?Future|(scala\\.util\\.)?Try|(scala\\.util\\.)?Either|cats\\.data\\.Validated):e",
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
