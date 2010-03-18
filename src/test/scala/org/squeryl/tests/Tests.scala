

package org.squeryl.tests

import _root_.org.squeryl.SessionFactory
import customtypes.{TestCustomTypesMode}
import musicdb.MusicDb
import org.squeryl.adapters.{MySQLAdapter, PostgreSqlAdapter, H2Adapter, OracleAdapter}
import schooldb.SchoolDb
import org.squeryl.{Session}
import java.sql.{Connection, DriverManager}


object Tests extends QueryTester {

  def main(args : Array[String]) : Unit = {

    //allTestsOnAllDatabases

    //dumpResourceClosingPolicyOfAllDrivers
    
    //org.squeryl.demos.KickTheTires.testWithH2

    allTestsOnH2
  }

  def allTestsOnH2 = {
    allTests("H2", createH2TestConnection _)
  }

  def allTestsOnAllDatabases = {


    allTests("Oracle", createOracleTestConnection _)

    allTests("PosgreSQL", createPostgreSqlTestConnection _)

    allTests("H2", createH2TestConnection _)

    allTests("MySQL", createMySQLTestConnection _)
  }

  def dumpResourceClosingPolicyOfAllDrivers = {

    //testConnectionClose(createOracleTestConnection)
    testConnectionClose(createPostgreSqlTestConnection)
    testConnectionClose(createMySQLTestConnection)
    testConnectionClose(createH2TestConnection)
  }

  def allTests(dbName: String, s: ()=>Session) = {

    println("Will run test suite with " + dbName)
    
    // tests that need no Db Session :
    (new AnnotationTests).testMetaData

    // tests that do :

    import org.squeryl.PrimitiveTypeMode._

    SessionFactory.concreteFactory = Some(s)
    
    transaction {
      (new SchoolDb).test1
    }

    inTransaction {
      inTransaction {
        (new MusicDb).test1
      }
    }

    val session = s()
    try {
      using(session) {                
        (new TestCustomTypesMode).testAll
      }

      org.squeryl.demos.KickTheTires.test(session)

      if(!session.connection.getAutoCommit)
        session.connection.commit
    }
    finally {
      //session.connection.rollback
      session.connection.close
    }
  }

  def testConnectionClose(session: Session) = {

    val stmt = session.connection.prepareStatement("select * from course")
    val rs = stmt.executeQuery

    session.connection.close

    println("When " + session.connection.getClass.getName + " closes :")
    println("statement is closed ? --> " + stmt.isClosed)
    println("resultSet is closed ? --> " + rs.isClosed)
  }


  def createH2TestConnection = {
    Class.forName("org.h2.Driver");

    Session.create(
      java.sql.DriverManager.getConnection("jdbc:h2:~/test", "sa", ""),
      new H2Adapter
    )
  }

  def createOracleTestConnection = {
    Class.forName("oracle.jdbc.OracleDriver");
    
    Session.create(
      java.sql.DriverManager.getConnection("jdbc:oracle:thin:@localhost:1521:xe", "squeryl", "squeryl"),
      new OracleAdapter
    )
  }

  def createPostgreSqlTestConnection = {
    Class.forName("org.postgresql.Driver");

    Session.create(
      java.sql.DriverManager.getConnection("jdbc:postgresql://localhost:5432/squeryl", "squeryl", "squeryl"),
      new PostgreSqlAdapter
    )
  }

  def createMySQLTestConnection = {
    Class.forName("com.mysql.jdbc.Driver");

    val c = DriverManager.getConnection("jdbc:mysql://localhost/test?user=squeryl&password=squeryl")

    //com.mysql.jdbc.Driver defaults to TRANSACTION_REPEATABLE_READ
    c.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED)
    
    Session.create(c,new MySQLAdapter)
  }
}