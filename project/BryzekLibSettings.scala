import org.scalafmt.sbt.ScalafmtPlugin.autoImport._
import sbt.Keys._
import sbt._
import xerial.sbt.Sonatype.autoImport._

/** The build settings every lib-* repository shares: publishing metadata, the guava pin, the
  * browser-stack exclusions, the compiler gate and the root project's test reporting.
  *
  * THIS FILE IS OWNED BY devops (templates/scala-libs/project/BryzekLibSettings.scala) AND COPIED
  * VERBATIM into each lib's `project/`. Each lib's `ci/build.sh` diffs its copy against devops's
  * `main` and fails on any difference, so a change here is one edit plus one copy per repo, and a
  * copy that is not taken fails that repo's `ci` rather than drifting. Edit it there, never here:
  *
  * {{{
  * dev repo cat devops templates/scala-libs/project/BryzekLibSettings.scala > project/BryzekLibSettings.scala
  * }}}
  *
  * Each build.sbt keeps what is its own -- `name`, `version`, `scalaVersion`, which pins from
  * BryzekPins apply to it, its dependencies and its extras -- and wires the shared settings in:
  *
  * {{{
  * BryzekLibSettings.buildSettings("lib-util")
  *
  * lazy val root = project
  *   .in(file("."))
  *   .settings(BryzekLibSettings.rootSettings)
  *   .settings(libraryDependencies ++= Seq(...))
  * }}}
  *
  * `scalaVersion` STAYS IN build.sbt. devops reads it out of the file's text (`Built_commit`,
  * `Scalafmt_policy`), so a value only sbt can see is one those checks silently stop asking about.
  *
  * sbt compiles `project/` before it reads build.sbt, so nothing here may name a value build.sbt
  * defines; the repository name arrives as an argument for that reason.
  */
object BryzekLibSettings {

  /** Build-wide settings: the publishing coordinates and metadata Maven Central requires, the guava
    * pin and the browser-stack exclusions. Placed at the top level of build.sbt.
    */
  def buildSettings(repo: String): Seq[Setting[_]] = publishing(repo) ++ Seq(
    ThisBuild / dependencyOverrides += guava,
    ThisBuild / excludeDependencies ++= browserStackExclusions
  )

  /** The root project's settings: formatting on compile, the docs jar, the compiler gate and the
    * per-suite test reports. A lib appends its own after these, so a `-Wconf` it adds lands after
    * the shared ones and wins over them.
    */
  val rootSettings: Seq[Setting[_]] = Seq(
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
    scalacOptions ++= compilerOptions
  )

  private def publishing(repo: String): Seq[Setting[_]] = Seq(
    ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17"),
    // The published groupId. Do not remove: nothing else sets it, and without it the
    // artifact publishes under a default groupId that no consumer resolves (lib-util 0.0.34).
    ThisBuild / organization := "com.bryzek",
    ThisBuild / homepage := Some(url(s"https://github.com/mbryzek/$repo")),
    ThisBuild / licenses := Seq("MIT" -> url(s"https://github.com/mbryzek/$repo/blob/main/LICENSE")),
    ThisBuild / developers := List(
      Developer("mbryzek", "Michael Bryzek", "mbryzek@alum.mit.edu", url("https://github.com/mbryzek"))
    ),
    ThisBuild / scmInfo := Some(
      ScmInfo(url(s"https://github.com/mbryzek/$repo"), s"scm:git@github.com:mbryzek/$repo.git")
    ),
    ThisBuild / publishTo := sonatypePublishToBundle.value,
    ThisBuild / sonatypeCredentialHost := "central.sonatype.com",
    ThisBuild / sonatypeRepository := "https://central.sonatype.com/api/v1/publisher"
  )

  // com.google.guava:guava resolves to 33.7.2-jre or above, in every subproject.
  //
  // Below 33.7.2 CompactHashMap, CompactHashSet and MapMakerInternalMap trust the size a serialized
  // stream states and allocate their backing arrays from it before reading a single element, so a
  // small hostile payload handed to Java deserialization makes the JVM allocate whatever it names and
  // die of OutOfMemoryError rather than reject the stream (GHSA-xxph-c9ww-hj94).
  //
  // No lib declares it: Play depends on it directly and pins 32.1.3-jre (play_3 where the build
  // enables PlayScala, play-test_3 where only `scalatestplus-play % Test` brings Play), so an
  // override is the only thing that moves it and there is no `libraryDependencies` line to edit
  // instead. Applied as a `dependencyOverrides`, which governs each build's own resolution and
  // writes nothing into the published POM. Drop this pin once the Play version the libs resolve
  // ships a guava at or above 33.7.2 of its own.
  val guava: ModuleID = "com.google.guava" % "guava" % "33.7.2-jre"

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
  // TestBrowser and WebDriverFactory, and no lib constructs a WebDriver, extends a
  // scalatestplus-play browser trait, or uses anything from it beyond PlaySpec,
  // GuiceOneAppPerSuite and GuiceOneServerPerSuite. A browser test written after this fails to
  // link, loudly, at the moment it is written -- which is the intended trade: add a maintained
  // stack (org.htmlunit, or Playwright) rather than inherit a stale one through a transitive.
  //
  // Build-wide, so sbt writes an <exclusions> block for each of these onto every dependency in
  // the published POM; that is the published shape these libs already have.
  val browserStackExclusions: Seq[ExclusionRule] = Seq(
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
  // tens of thousands of statements whose only legal fix this rule has ruled out. A lib that
  // silences a path of its own (lib-ai's committed apibuilder client) appends its `-Wconf` after
  // these, in its own build.sbt.
  //
  // `-Xlint` IS NOT IN THIS SET, and its Scala 3 replacement is why. The option is Scala 2's;
  // Scala 3 deprecates it and schedules it for removal, pointing at `-Wshadow` instead -- and
  // `-Wshadow` fires on a constructor parameter forwarded to a base class that re-exposes it as a
  // `val` of the same name, which is the only spelling that relationship has. Its remedy is a
  // worse parameter name at every named-argument call site, so the lint costs more than the
  // shadowing it reports.
  val compilerOptions: Seq[String] = Seq(
    "-feature",
    "-Werror",
    "-Wimplausible-patterns",
    "-Wnonunit-statement",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wconf:id=E175:s",
    "-Wconf:id=E176:s",
    "-Wconf:msg=value of type ((scala\\.concurrent\\.)?Future|(scala\\.util\\.)?Try|(scala\\.util\\.)?Either|cats\\.data\\.Validated):e"
  )
}
