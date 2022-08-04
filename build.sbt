name := "squeryl"

description := "A Scala ORM and DSL for talking with Databases using minimum verbosity and maximum type safety"

organization := "org.squeryl"

version := "0.9.18"

javacOptions := {
  if (scala.util.Properties.isJavaAtLeast("17")) {
    Seq("-source", "1.8", "-target", "1.8")
  } else {
    Seq("-source", "1.6", "-target", "1.6")
  }
}

Test / fork := true

// https://github.com/squeryl/squeryl/issues/340
// TODO remove this workaround
Test / javaOptions ++= {
  if (scala.util.Properties.isJavaAtLeast("11")) {
    Seq("--add-opens=java.base/java.lang=ALL-UNNAMED")
  } else {
    Nil
  }
}

//only release *if* -Drelease=true is passed to JVM
version := {
  val v = version.value
  val release = Option(System.getProperty("release")) == Some("true")
  if(release)
    v
  else {
    val suffix = Option(System.getProperty("suffix"))
    val i = (v.indexOf('-'), v.length) match {
      case (x, l) if x < 0 => l
      case (x, l) if v substring (x+1) matches """\d+""" => l //patch level, not RCx
      case (x, _) => x
    }
    v.substring(0,i) + "-" + (suffix getOrElse "SNAPSHOT")
  }
}

parallelExecution := false

publishMavenStyle := true

val Scala211 = "2.11.12"

scalaVersion := Scala211

crossScalaVersions := Seq("2.12.16", Scala211, "2.10.7", "2.13.8")

Compile / doc / scalacOptions ++= {
  val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
  val hash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
  Seq("-sourcepath", base, "-doc-source-url", "https://github.com/squeryl/squeryl/tree/" + hash + "€{FILE_PATH}.scala")
}

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v >= 11 =>
      Seq(
        "-Xsource:3",
      )
    case _ =>
      Nil
  }
}

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-language:existentials"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      Seq("-Xfuture")
    case _ =>
      Nil
  }
}

val unusedWarnings = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) =>
      Nil
    case Some((2, 11)) =>
      Seq("-Ywarn-unused-import")
    case _ =>
      Seq("-Ywarn-unused:imports")
  }
)

scalacOptions ++= unusedWarnings.value

Seq(Compile, Test).flatMap(c =>
  c / console / scalacOptions --= unusedWarnings.value
)

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://squeryl.org"))

pomExtra := (<scm>
               <url>git@github.com:squeryl/squeryl.git</url>
               <connection>scm:git:git@github.com:squeryl/squeryl.git</connection>
             </scm>
             <developers>
               <developer>
                 <id>max-l</id>
                 <name>Maxime Lévesque</name>
                 <url>https://github.com/max-l</url>
               </developer>
               <developer>
                 <id>davewhittaker</id>
                 <name>Dave Whittaker</name>
                 <url>https://github.com/davewhittaker</url>
               </developer>
             </developers>)

publishTo := sonatypePublishToBundle.value

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "3.3.0",
  "com.h2database" % "h2" % "1.4.200" % "provided",
  "mysql" % "mysql-connector-java" % "8.0.30" % "provided",
  "org.postgresql" % "postgresql" % "42.4.1" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "provided",
  "org.apache.derby" % "derby" % "10.16.1.1" % "provided",
  "org.xerial" % "sqlite-jdbc" % "3.39.2.0" % "test",
  "org.json4s" %% "json4s-scalap" % "3.6.12",
)

libraryDependencies ++= {
  Seq("org.scalatest" %% "scalatest" % "3.2.13" % "test")
}

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.3.0")
    case Some((2, scalaMajor)) if scalaMajor >= 12 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "2.1.0")
    case _ =>
      Nil
  }
}
