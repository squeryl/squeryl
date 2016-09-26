name := "squeryl"

description := "A Scala ORM and DSL for talking with Databases using minimum verbosity and maximum type safety"

organization := "org.squeryl"

version := "0.9.6-RC3"

javacOptions := Seq("-source", "1.6", "-target", "1.6")

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

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.12.0-RC1", "2.11.6", "2.10.5")

scalacOptions ++= {
  Seq("-unchecked", "-deprecation") ++ (
  if(scalaVersion.value.startsWith("2.11"))
    Seq("-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:existentials")
  else
    Nil
  )
}

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("http://squeryl.org"))

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

credentials ~= { c =>
  (Option(System.getenv().get("SONATYPE_USERNAME")), Option(System.getenv().get("SONATYPE_PASSWORD"))) match {
    case (Some(username), Some(password)) =>
      c :+ Credentials(
        "Sonatype Nexus Repository Manager",
        "oss.sonatype.org",
        username,
        password)
    case _ => c
  }
}

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "2.2",
  "com.h2database" % "h2" % "1.4.187" % "provided",
  "mysql" % "mysql-connector-java" % "5.1.35" % "provided",
  "postgresql" % "postgresql" % "9.1-901.jdbc4" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.2.4" % "provided",
  "org.apache.derby" % "derby" % "10.11.1.1" % "provided",
  "org.xerial" % "sqlite-jdbc" % "3.8.7" % "test",
  "junit" % "junit" % "4.8.2" % "provided",
  "org.json4s" %% "json4s-scalap" % "3.4.1",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.5")
    case _ =>
      Nil
  }
}
