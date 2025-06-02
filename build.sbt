import sbtrelease.ReleaseStateTransformations.*

import sbt.*
import sbt.Keys.*

val Scala211 = "2.11.12"
val Scala213 = "2.13.16"

wartremoverWarnings ++= Warts.unsafe

name := "squeryl"

description := "A Scala ORM and DSL for talking with Databases using minimum verbosity and maximum type safety"

val ivyLocal = Resolver.file("ivyLocal", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)
val nexus = "Sonatype Nexus Repository Manager" at "https://nexus.criterionhcm.com/repository/maven-public/"
val resolutionRepos = Seq(nexus, ivyLocal, Resolver.mavenLocal)

val commonSettings = Def.settings(
  organization := "org.squeryl",
  javacOptions := {
    if (scala.util.Properties.isJavaAtLeast("17")) {
      Seq("-source", "1.8", "-target", "1.8")
    } else if (scala.util.Properties.isJavaAtLeast("11")) {
      Seq("-source", "1.7", "-target", "1.7")
    } else {
      Seq("-source", "1.6", "-target", "1.6")
    }
  },
  Test / fork := true,
  Test / javaOptions ++= {
    // https://github.com/squeryl/squeryl/issues/340
    // TODO remove this workaround
    if (scala.util.Properties.isJavaAtLeast("11")) {
      Seq("--add-opens=java.base/java.lang=ALL-UNNAMED")
    } else {
      Nil
    }
  },
  releaseCrossBuild := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+ publishSigned"),
    releaseStepCommandAndRemaining("sonaRelease"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  ),
  parallelExecution := false,
  publishMavenStyle := true,
  crossScalaVersions := Seq("2.12.20", Scala211, "2.10.7", Scala213, "3.3.6"),
  Compile / doc / scalacOptions ++= {
    val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
    val hash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
    Seq(
      "-sourcepath",
      base,
      "-doc-source-url",
      "https://github.com/squeryl/squeryl/tree/" + hash + "€{FILE_PATH}.scala"
    )
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        Seq(
          "-Xsource:3-cross",
        )
      case Some((2, 12 | 11)) =>
        Seq(
          "-Xsource:3",
        )
      case _ =>
        Nil
    }
  },
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:existentials"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq("-Xfuture")
      case _ =>
        Nil
    }
  },
  scalacOptions ++= unusedWarnings.value,
  Seq(Compile, Test).flatMap(c => c / console / scalacOptions --= unusedWarnings.value),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://squeryl.org")),
  organizationName := "Criterion Inc.",
  resolvers += nexus,
  externalResolvers := Resolver.combineDefaultResolvers(resolvers.value.toVector, mavenCentral = false),
  publishTo := Some(
    "Nexus Realm" at "https://nexus.criterionhcm.com/nexus/content/groups/criterionhcm/squeryl"
  ),
  credentials += Credentials(new File(baseDirectory.value, ".nexus_credentials")),
  Test / publishArtifact := false,
  scalaVersion := Scala213
)

commonSettings

lazy val unusedWarnings = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) =>
      Nil
    case Some((2, 11)) =>
      Seq("-Ywarn-unused-import")
    case Some((2, 12)) =>
      Seq("-Ywarn-unused:imports")
    case _ =>
      Seq("-Wunused:imports")
  }
)

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "3.3.0",
  "com.h2database" % "h2" % "1.4.200" % "provided",
  "com.mysql" % "mysql-connector-j" % "9.3.0" % "provided",
  "org.postgresql" % "postgresql" % "42.7.6" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "provided",
  "org.apache.derby" % "derby" % "10.11.1.1" % "provided",
  "org.xerial" % "sqlite-jdbc" % "3.39.3.0" % "test",
  "org.scalatest" %% "scalatest-funsuite" % "3.2.19" % "test",
  "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.19" % "test",
)

libraryDependencies ++= {
  val scalap = "org.json4s" %% "json4s-scalap" % "3.6.12"
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((scalaMajor, scalaMinor)) if scalaMajor == 3 =>
      Seq(
        "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
      )
    case Some((scalaMajor, scalaMinor)) if scalaMajor == 2 && scalaMinor >= 12 =>
      Seq(
        "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
        scalap
      )
    case Some((scalaMajor, scalaMinor)) if scalaMajor == 2 && scalaMinor >= 11 =>
      Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.3.1",
        scalap
      )
    case Some((scalaMajor, scalaMinor)) if scalaMajor == 2 && scalaMinor >= 10 =>
      Seq(scalap)
    case _ =>
      Nil
  }
}

val disableMacrosProject: Def.Initialize[Boolean] = Def.setting(
  scalaBinaryVersion.value != "3"
)

lazy val macros = project
  .in(file("macros"))
  .settings(
    commonSettings,
    publish / skip := disableMacrosProject.value,
  )

dependsOn(macros)

ThisBuild / semanticdbEnabled := {
  scalaBinaryVersion.value != "2.10"
}

ThisBuild / semanticdbVersion := {
  scalaBinaryVersion.value match {
    case "2.11" =>
      "4.8.10"
    case _ =>
      "4.12.3"
  }
}

Compile / sourceGenerators += task {
  val dir = (Compile / sourceManaged).value
  val size = 22
  (Seq(
    "JoinSignatures.scala" -> JoinSignatures.value(size),
    "FromSignatures.scala" -> FromSignatures.value(size),
    "STuple.scala" -> STuple.value(size),
    "OrderBySignatures.scala" -> OrderBySignatures.value(size),
    "GroupBySignatures.scala" -> GroupBySignatures.value(size),
    "ComputeMeasuresSignaturesFromStartOrWhereState.scala" -> ComputeMeasuresSignaturesFromStartOrWhereState.value(
      size
    ),
    "ComputeMeasuresSignaturesFromGroupByState.scala" -> ComputeMeasuresSignaturesFromGroupByState.value(size),
    "Query.scala" -> Query.value(size),
  ).map { case (fileName, value) =>
    val f = dir / "org" / "squeryl" / "dsl" / "boilerplate" / fileName
    f -> value
  } ++ Seq(
    "CompositeKeyN.scala" -> CompositeKeyN.value(size),
    "QueryYieldMethods.scala" -> QueryYieldMethods.value(size),
  ).map { case (fileName, value) =>
    val f = dir / "org" / "squeryl" / "dsl" / fileName
    f -> value
  }).map { case (f, value) =>
    IO.write(f, value)
    f
  }
}

Compile / packageSrc / mappings ++= (Compile / managedSources).value.map { f =>
  // to merge generated sources into sources.jar as well
  (f, f.relativeTo((Compile / sourceManaged).value).get.getPath)
}
