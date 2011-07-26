
name := "squeryl"

organization := "org.squeryl"

version := "0.9.5-Beta"

scalaVersion := "2.9.0-1"

crossScalaVersions := Seq("2.9.0-1","2.9.0","2.8.1","2.8.0")

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "2.2",
  "com.h2database" % "h2" % "1.2.127" % "provided",
  "mysql" % "mysql-connector-java" % "5.1.10" % "provided",
  "postgresql" % "postgresql" % "8.4-701.jdbc4" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.2.4" % "provided",
  "org.apache.derby" % "derby" % "10.7.1.1" % "provided",
  "junit" % "junit" % "4.8.2" % "provided"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scalap" % _ % "provided")
  
libraryDependencies <+= scalaVersion(sv=> sv match {
     case "2.9.0-1" => 
	   "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "provided"	 
	 case v:String =>
	   if(! v.startsWith("2.8")) 
	     "org.scalatest" % v % "scalatest" % "1.4.1" % "provided"
	   else
	     "org.scalatest" % "scalatest_2.8.0" % "1.3.1.RC2" % "provided"
  })

retrieveManaged := true  

parallelExecution := false

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  if(v endsWith "-SNAPSHOT")
    Some(ScalaToolsSnapshots)
  else
    Some(ScalaToolsReleases)
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

pomExtra :=
    <licenses>
      <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      </license>
    </licenses>
