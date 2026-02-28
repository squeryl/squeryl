addSbtPlugin("com.github.xuwei-k" % "sbt-root-aggregate" % "0.1.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.6")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")

conflictWarning := ConflictWarning("warn", Level.Warn, false)
