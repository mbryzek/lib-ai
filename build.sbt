name := "lib-ai"

version := "0.2.85"

// Publishing metadata, the guava pin, the browser-stack exclusions, the compiler gate and the
// root project's test reporting are shared by every lib-* repository and DECLARED in
// BryzekLibSettings, published by mbryzek/lib-build as lib-build-sbt (project/plugins.sbt). That
// source carries why each setting is what it is; a change is released there and bumped here.
BryzekLibSettings.buildSettings("lib-ai")

ThisBuild / scalaVersion := "3.10.0"

// lib-build-testkit (the pin suites, Test scope) is published to the Bryzek Maven repository, not
// Maven Central. project/plugins.sbt names the same resolver for the meta-build.
ThisBuild / resolvers += "Bryzek Maven repository" at "https://maven.bryzek.com"

// The Jackson 2, Jackson 3 and logback security pins are DECLARED in BryzekPins, published by
// mbryzek/lib-build as lib-build-sbt (project/plugins.sbt), and asserted against this build's own
// resolution by the pin specs under test/scala/com/bryzek/pins, subclasses of lib-build-testkit's
// suites. That source carries why each floor is what it is; an advisory bump is released there
// and bumped here.
//
// Applied as `dependencyOverrides`, which governs THIS build's resolution only -- sbt writes none
// of it into the published POM -- so it decides what this repo compiles and tests against and
// imposes no floor on a consumer. No Jackson 3 resolves here. logback is
// both DECLARED (below, so the floor reaches consumers) and overridden, so the pair cannot split.
ThisBuild / dependencyOverrides ++= BryzekPins.jackson2 ++ BryzekPins.logback

// at.yawk.lz4:lz4-java resolves to 1.11.4 or above, in every subproject.
//
// Below 1.11.4 three defects reach a caller: LZ4BlockInputStream with stopOnEmptyBlock=false
// recurses once per empty block, so a stream of empty blocks ends in a StackOverflowError
// (GHSA-343h-94h5-c4wr); LZ4FrameInputStream reallocates its block buffers for every frame, so a
// small input of many frames amplifies CPU and GC (GHSA-gm45-99xc-r7wv); and the native library
// is extracted to a shared temporary directory where another local user can replace it before
// it is loaded (GHSA-mcr4-qmvw-px4g).
//
// Below 1.11.1 the JNI-backed XXHash implementations hand a caller-supplied byte array and its
// `off`/`len` to native code without first proving the range is inside the array: the streaming
// `update` paths validate nothing at all, and the one-shot `hash` paths call
// `SafeUtils.checkRange`, which skips every array access when `len == 0` and so lets a null array
// through. Native code then dereferences it and the JVM dies where a Java exception belongs
// (GHSA-xx22-p4ch-683r). The pure-Java XXHash implementations are unaffected, and so is the
// ordinary case where only a valid array's CONTENTS are attacker-influenced -- the exposure is the
// array reference and the range arguments.
//
// It is not declared anywhere in this build: play_3 depends on it directly and pins 1.11.0, so an
// override is the only thing that moves it and there is no `libraryDependencies` line to edit
// instead. Drop this pin once the Play version this build resolves ships a lz4-java at or above
// 1.11.4 of its own.
ThisBuild / dependencyOverrides += "at.yawk.lz4" % "lz4-java" % "1.11.4"

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayScala)
  .settings(BryzekLibSettings.rootSettings)
  .settings(
    // ISS-2173: the Play plugin sets `Test / fork := true`, and a forked JVM inherits none of
    // `SBT_OPTS` -- left unset its max heap is a QUARTER OF PHYSICAL RAM, which is 16G on the 64G
    // laptop and 6G on the 24G mini. Without this pin the CI heap measurement behind
    // `-Xmx4G` in `.jvmopts` would be a fact about whichever machine happened to run
    // it rather than about this suite. platform and acumen pin theirs for the same reason.
    Test / javaOptions += "-Xmx4g",
    // The committed apibuilder client under `app/scala/com/bryzek/ai/generated/` is
    // regenerated, not hand-written, so a warning there is a codegen bug rather than a
    // bug in this repo and `-Werror` must not fail this build for one. Appended after the
    // shared gate in rootSettings, so this later `-Wconf` wins over it.
    scalacOptions += "-Wconf:src=.*/generated/.*:s",
    // logback-classic and logback-core, at `BryzekPins.logbackVersion`, DECLARED rather than only
    // overridden.
    //
    // `PlayScala` puts play-logback on this library at compile scope, so it is in the published POM
    // and logback reaches every consumer through it, at whatever version play-logback names.
    // Declaring the pair keeps the floor this library's own statement rather than a property of
    // whichever Play release is pinned in plugins.sbt, and keeps core and classic on one version. A
    // `dependencyOverrides` is resolution-local, so on its own it would fix this build's classpath
    // and leave every consumer inheriting the advisory from a library that reads as fixed; a direct
    // compile-scope dependency is what reaches them.
    libraryDependencies ++= BryzekPins.logback,
    libraryDependencies ++= Seq(
      ws,
      "joda-time" % "joda-time" % "2.15.1",
      "com.google.inject" % "guice" % "5.1.0",
      "org.playframework" %% "play-json" % "3.0.6",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test,
      // The suites the pin specs under test/scala/com/bryzek/pins subclass. Every dependency of
      // it is Provided, so each suite runs against what this build resolves. Same version as lib-build-sbt in
      // project/plugins.sbt.
      "com.bryzek" %% "lib-build-testkit" % "0.0.1" % Test
    )
  )
