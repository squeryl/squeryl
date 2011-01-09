/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.tests

import _root_.org.squeryl.demos.MusicDb
import _root_.org.squeryl.SessionFactory
import customtypes.{TestCustomTypesMode}
import musicdb.{MusicDbTestRun, MusicDb}
import mutablerelations.SchoolDb2MetableRelations
import org.squeryl.{Session}
import java.sql.{Connection, DriverManager}
import schooldb.{SchoolDbTestRun, Issue14, SchoolDb}
import org.squeryl.adapters._
import schooldb2.SchoolDb2Tests


object Tests extends QueryTester {

  def main(args : Array[String]) : Unit = {
    //dumpSchemasForAllDatabases
    //allTestsOnAllDatabases

    //allTests("PosgreSQL", createPostgreSqlTestConnection _)
    
    //dumpResourceClosingPolicyOfAllDrivers
    
    //org.squeryl.demos.KickTheTires.testWithH2

    allTestsOnH2

    //dumpSchemasForAllDatabases

    TransactionsTests.allTests(() =>createH2TestConnection)
    
    //leakTest    
  }

  def leakTest = {

    SessionFactory.concreteFactory = Some(()=>createPostgreSqlTestConnection)

    import org.squeryl.PrimitiveTypeMode._
    
    val mdb = transaction {
      new MusicDbTestRun
    }

    mdb.leakTest
  }

  def issue14Test = {

    import org.squeryl.PrimitiveTypeMode._

    SessionFactory.concreteFactory = Some(createOracleTestConnection _)
    (new Issue14).testIssue14
  }
  
  def allTestsOnH2 = {
    allTests("H2", createH2TestConnection _)
  }

  def allTestsOnDB2 = {
    allTests("DB2", createDB2TestConnection _)
  }

  def allTestsOnAllDatabases = {

    allTests("MS Sql", createMSSqlTestConnection _)

    allTests("Oracle", createOracleTestConnection _)
    
    allTests("PosgreSQL", createPostgreSqlTestConnection _)
    
    allTests("MySQL", createMySQLTestConnection _)

    issue14Test

    allTests("H2", createH2TestConnection _)

    allTests("DB2", createDB2TestConnection _)
  }

  def dumpResourceClosingPolicyOfAllDrivers = {

    testConnectionClose(createPostgreSqlTestConnection)
    testConnectionClose(createOracleTestConnection)
    testConnectionClose(createMySQLTestConnection)
    testConnectionClose(createH2TestConnection)
    testConnectionClose(createDB2TestConnection)
  }

  def allTests(dbName: String, s: ()=>Session) = {

    println("Will run test suite with " + dbName)
    
    // tests that need no Db Session :
    (new AnnotationTests).testMetaData

    // tests that do :

    import org.squeryl.PrimitiveTypeMode._

    SessionFactory.concreteFactory = Some(s)

    transaction {

      (new SchoolDb2MetableRelations).testAll
      
      (new SchoolDb2Tests).testAll

      (new SchoolDbTestRun).test1
    }

    inTransaction {
      inTransaction {
        (new MusicDbTestRun).test1
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
    //java.sql.DriverManager.getConnection("jdbc:h2:mem:", "", ""),
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
    val c = java.sql.DriverManager.getConnection("jdbc:postgresql://localhost:5432/squeryl", "squeryl", "squeryl")
    c.setAutoCommit(false)
    Session.create(c, new PostgreSqlAdapter)
  }

  def createMySQLTestConnection = {
    Class.forName("com.mysql.jdbc.Driver");

    val c = DriverManager.getConnection("jdbc:mysql://localhost/test?user=squeryl&password=squeryl")

    //com.mysql.jdbc.Driver defaults to TRANSACTION_REPEATABLE_READ
    c.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED)
    
    Session.create(c,new MySQLAdapter)
  }

  def createMSSqlTestConnection = {

      Class.forName("net.sourceforge.jtds.jdbc.Driver")
      //val c = DriverManager.getConnection("jdbc:jtds:sqlserver://localhost:1433/SQLEXPRESS;user=sa;password=zaza")
      //val c = DriverManager.getConnection("jdbc:jtds:sqlserver://localhost/SQLEXPRESS","sa","zaza")

      //val c = DriverManager.getConnection("jdbc:jtds:sqlserver://localhost/SQLEXPRESS;user=sa;password=zaza")

      val c = DriverManager.getConnection("jdbc:jtds:sqlserver://localhost:1433/squeryl;instance=SQLEXPRESS;user=sa;password=zaza;prepareSQL=1")

      Session.create(c, new MSSQLServer )    
  }

  def createDB2TestConnection = {
      Class.forName("com.ibm.db2.jcc.DB2Driver")
      val c = DriverManager.getConnection("jdbc:db2://localhost:50000/squeryl", "squeryl", "squeryl")

      Session.create(c, new DB2Adapter )
  }


  def dumpSchemasForAllDatabases = {
    
    dumpSchemas("-----------Postgres Schema",createPostgreSqlTestConnection _)
    dumpSchemas("-----------Oracle Schema",createOracleTestConnection _)
    dumpSchemas("-----------MySql Schema",createMySQLTestConnection _)
    dumpSchemas("-----------H2 Schema",createH2TestConnection _)
    dumpSchemas("-----------MSSql Schema",createMSSqlTestConnection _)
  }

  def dumpSchemas(dbName: String, s: ()=>Session)= {

    val session = s()

    import org.squeryl.PrimitiveTypeMode._
    
    using(session) {
      (new MusicDb).printDdl

      (new SchoolDb).printDdl

      (new org.squeryl.tests.schooldb2.SchoolDb2).printDdl
    }

  }
}
