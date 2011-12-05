package org.squeryl.mssql

import org.squeryl.test._
import org.squeryl.adapters.MSSQLServer
import org.squeryl.framework.DBConnector
import org.squeryl.Session

trait MSSQL_Connection extends DBConnector{
  def connectToDb() : Option[() => Session] = {
    if(config.hasProps("mssql.connectionString")) {
      Class.forName("net.sourceforge.jtds.jdbc.Driver")

      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("mssql.connectionString")
        )
        c.setAutoCommit(false)
        Session.create(c, new MSSQLServer)
      })
    }else{
      None
    }
  }
}

class MSSQL_UuidTests extends UuidTests with MSSQL_Connection
class MSSQL_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with MSSQL_Connection
class MSSQL_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with MSSQL_Connection
class MSSQL_TransactionTests extends TransactionTests with MSSQL_Connection
class MSSQL_SchoolDb2 extends schooldb2.SchoolDb2Tests with MSSQL_Connection
class MSSQL_SchoolDb extends schooldb.SchoolDbTestRun with MSSQL_Connection {
  override val ignoredTests = List("OuterJoinMixed1")
}
//class MSSQL_TestCustomTypesMode extends customtypes.TestCustomTypesMode with MSSQL_Connection
class MSSQL_KickTheTires extends demo.KickTheTires with MSSQL_Connection
class MSSQL_MusicDb extends musicdb.MusicDbTestRun with MSSQL_Connection {
  override val ignoredTests = List("PaginatedQuery1", "OuterJoinMixed1")
}
class MSSQL_LeftJoinTest extends LeftJoinTest with MSSQL_Connection

//class MSSQL_ConnectionClosing extends ConnectionClosingTest with MSSQL_Connection {
//  def dbSpecificSelectNow: String = "SELECT CURRENT_TIMESTAMP"
//}
