logLevel := Level.Warn

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.12")

// BryzekPins, BryzekLibSettings and the test-health plugins (CiTestListsPlugin, CiTestRerunPlugin),
// published by mbryzek/lib-build. Its README is the reference.
resolvers += "Bryzek Maven repository" at "https://maven.bryzek.com"
libraryDependencies += "com.bryzek" %% "lib-build-sbt" % "0.0.1"
