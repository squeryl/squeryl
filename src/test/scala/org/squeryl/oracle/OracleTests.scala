package org.squeryl.oracle


import org.squeryl.test._

import org.squeryl.framework.DBConnector
import org.squeryl.adapters.OracleAdapter

import org.squeryl.Session

trait Oracle_Connection extends DBConnector{
  def connectToDb() : Option[() => Session] = {
    if(config.hasProps("oracle.connectionString", "oracle.user", "oracle.password")){
      Class.forName("oracle.jdbc.OracleDriver")

      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("oracle.connectionString"),
          config.getProp("oracle.user"),
          config.getProp("oracle.password")
        )
        c.setAutoCommit(false)
        Session.create(c, new OracleAdapter)
      })
    }else{
      None
    }
  }
}

class Oracle_UuidTests extends UuidTests with Oracle_Connection
//class Oracle_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with Oracle_Connection
class Oracle_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with Oracle_Connection
class Oracle_TransactionTests extends TransactionTests with Oracle_Connection
class Oracle_SchoolDb2 extends schooldb2.SchoolDb2Tests with Oracle_Connection
class Oracle_SchoolDb extends schooldb.SchoolDbTestRun with Oracle_Connection{
  override val ignoredTests = List("OuterJoinMixed1")
}
//class Oracle_TestCustomTypesMode extends customtypes.TestCustomTypesMode with Oracle_Connection
class Oracle_KickTheTires extends demo.KickTheTires with Oracle_Connection
class Oracle_MusicDb extends musicdb.MusicDbTestRun with Oracle_Connection {
  override val ignoredTests = List("testSQLMatchCaseNumericalWithOption2Numerical")
}
//class Oracle_LeftJoinTest extends LeftJoinTest with Oracle_Connection

//class Oracle_ConnectionClosing extends ConnectionClosingTest with Oracle_Connection {
//  def dbSpecificSelectNow: String = "select sysdate from dual"
//}
