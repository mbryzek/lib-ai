logLevel := Level.Warn

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.12.2")

addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.9")
