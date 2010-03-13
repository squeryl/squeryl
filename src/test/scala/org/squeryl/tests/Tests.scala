

package org.squeryl.tests

import _root_.org.squeryl.SessionFactory
import customtypes.{TestCustomTypesMode}
import musicdb.MusicDb
import java.sql.DriverManager
import org.squeryl.adapters.{MySQLAdapter, PostgreSqlAdapter, H2Adapter, OracleAdapter}
import schooldb.SchoolDb
import org.squeryl.{Session}


object Tests extends QueryTester {

  def main(args : Array[String]) : Unit = {

    //allTestsOnAllDatabases
    
    //org.squeryl.demos.KickTheTires.testWithH2

    allTestsOnH2
  }

  def allTestsOnH2 = {
    println("Tests with H2")
    allTests(createH2TestConnection)
  }

  def allTestsOnAllDatabases = {

    println("Tests with MySQL")
    allTests(createMySQLTestConnection)

    println("Tests with PosgreSQL")
    allTests(createPostgreSqlTestConnection)
    
    println("Tests with Oracle")
    allTests(createOracleTestConnection)

    println("Tests with H2")
    allTests(createH2TestConnection)    
  }

  def allTests(s: =>Session) = {

    // tests that need no Db Session :
    (new AnnotationTests).testMetaData

    // tests that do :
    val session = s

    import org.squeryl.PrimitiveTypeMode._

    SessionFactory.concreteFactory = Some(()=>s)
    
    transaction {
      (new SchoolDb).test1
    }

    inTransaction {
      inTransaction {
        (new MusicDb).test1
      }
    }

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

    Session.create(
      DriverManager.getConnection("jdbc:mysql://localhost/test?" +
                                 "user=squeryl&password=squeryl"),
      new MySQLAdapter
    )
  }
}