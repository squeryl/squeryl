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
 ******************************************************************************/
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

    allTests("MySQL", createMySQLTestConnection _)

    allTests("Oracle", createOracleTestConnection _)

    allTests("PosgreSQL", createPostgreSqlTestConnection _)

    allTests("H2", createH2TestConnection _)
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
