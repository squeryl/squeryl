
import sbt._

import com.weiglewilczek.bnd4sbt.BNDPlugin

class SquerylProject(info: ProjectInfo) extends DefaultProject(info) with BNDPlugin{
  
  val snapshot = systemOptional("snapshot", false).value
  
  override def version = {
    super.version match{
      case BasicVersion(major, minor, micro, extra) if snapshot =>
        BasicVersion(major, minor, micro, Some("SNAPSHOT"))
      case other => other
    }
  }
  
  val publishTo = 
    if(snapshot)
      "Scala Tools Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
    else
      "Scala Tools Release" at "http://nexus.scala-tools.org/content/repositories/releases/"
  
  override def managedStyle = ManagedStyle.Maven
  
  override def packageSrcJar = defaultJarPath("-sources.jar")
  
  val sourceArtifact = Artifact.sources(artifactID)
  
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)
    
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  
  override def pomExtra =
    <licenses>
      <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      </license>
    </licenses>
	
  /**
   * The following jars are the only runtime dependencies of Squeryl
   */
  
  val cglib = "cglib" % "cglib-nodep" % "2.2"

  val scalap = "org.scala-lang" % "scalap" % crossScalaVersionString

  /**
   * The following jars are for running the automated tests
   */
  
  val h2 = "com.h2database" % "h2" % "1.2.127" % "provided"
  
  val mysqlDriver = "mysql" % "mysql-connector-java" % "5.1.10" % "provided"
	
  val posgresDriver = "postgresql" % "postgresql" % "8.4-701.jdbc4" % "provided"
  
  val msSqlDriver = "net.sourceforge.jtds" % "jtds" % "1.2.4" % "provided"

  val derbyDriver = "org.apache.derby" % "derby" % "10.7.1.1" % "provided"

  val snapshotsRepo = "snapshots-repo" at "http://www.scala-tools.org/repo-snapshots"
    
  val junit = "junit" % "junit" % "4.8.2" % "provided"
  
  val scalatest = 
    if(crossScalaVersionString.startsWith("2.9.0-1")) 
      "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "provided"  
    else if(!crossScalaVersionString.startsWith("2.8")) 
      "org.scalatest" %% "scalatest" % "1.4.1" % "provided"
    else
      "org.scalatest" % "scalatest_2.8.0" % "1.3.1.RC2" % "provided"

   // we need to explictly include the scala package we use because a "scala.*" wildcard it will include
   // the scala.tools.scalap package we are explictly trying to not import in all cases
   override def bndImportPackage =
     "scala" ::
     "scala.collection.*" ::
     "scala.reflect.*" ::
     "scala.runtime.*" ::
     "scala.math" ::
     "scala.util.matching" ::
     "scala.xml" ::
     "net.sf.cglib.proxy.*"  :: Nil
   override def bndDynamicImportPackage = "scala.tools.scalap.scalax.rules.scalasig.*;resolution=optional" :: Nil
   override def bndExportPackage = "org.squeryl.*" :: Nil
   override def bndNoUses = true
   override def bndPrivatePackage = Nil
   override def bndBundleName = "Squeryl ORM"
}
