package org.squeryl.derby

import org.squeryl.test._

import org.squeryl.framework.DBConnector
import org.squeryl.adapters.DerbyAdapter

import org.squeryl.Session

trait Derby_Connection extends DBConnector{
  def connectToDb() : Option[() => Session] = {
    if(config.hasProps("derby.connectionString", "derby.user", "derby.password")){
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("derby.connectionString"),
          config.getProp("derby.user"),
          config.getProp("derby.password")
        )
        c.setAutoCommit(false)
        Session.create(c, new DerbyAdapter)
      })
    }else{
      None
    }
  }
}

class Derby_UuidTests extends UuidTests with Derby_Connection
class Derby_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with Derby_Connection
class Derby_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with Derby_Connection
class Derby_TransactionTests extends TransactionTests with Derby_Connection
class Derby_SchoolDb2 extends schooldb2.SchoolDb2Tests with Derby_Connection
class Derby_SchoolDb extends schooldb.SchoolDbTestRun with Derby_Connection {
  /* FIXME: https://issues.apache.org/jira/browse/DERBY-4998
   * Because of a derby bug we ignore the BigDecimal test cases until it's fixed.
   */
  override val ignoredTests = List("BigDecimal","assertColumnNameChangeWithDeclareSyntax")
}
//class Derby_TestCustomTypesMode extends customtypes.TestCustomTypesMode with Derby_Connection
class Derby_KickTheTires extends demo.KickTheTires with Derby_Connection
class Derby_MusicDb extends musicdb.MusicDbTestRun with Derby_Connection {

  override val ignoredTests = List(
    "testSQLCaseNumerical1",
    "testSQLCaseNumerical2",
    "testSQLCaseNumerical3",
    "testSQLCaseNonNumerical1",
    "testSQLCaseNonNumerical2",
    "testSQLCaseNonNumerical3",
    "testSQLMatchCaseNonNumerical2NonNumerical",
    "testSQLMatchCaseNonNumerical2Numerical",
    "testSQLMatchCaseNumerical2Numerical",
    "testSQLMatchCaseNumericalWithOption2Numerical",
    "testSQLMatchCaseEnemerationWitEnemeration",
    "testSQLMatchCaseEnemerationWithOption2Numerical"    
  )
}

class Derby_LeftJoinTest extends LeftJoinTest with Derby_Connection



