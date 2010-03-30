
import sbt._


class SquerylProject(info: ProjectInfo) extends DefaultProject(info) {
  
  override def managedStyle = ManagedStyle.Maven
  
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"

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
   * CGLIB is Squeryl's only dependency
   */
  
  val cglib = "cglib" % "cglib-nodep" % "2.2"

  /**
   * The following jars are for running the automated tests
   */
  
  val h2 = "com.h2database" % "h2" % "1.2.127"  
  
  val mysqlDriver = "mysql" % "mysql-connector-java" % "5.1.10"
	
  // Oracle jars cannot be distributed for "business" reasons ...
  //val oracleDriver = "com.oracle" % "ojdbc14" % "10.2.0.3.0"

  val posgresDriver = "postgresql" % "postgresql" % "8.4-701.jdbc4"
}