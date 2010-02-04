
import sbt._

class SquerylProject(info: ProjectInfo) extends DefaultProject(info) {
  
  
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