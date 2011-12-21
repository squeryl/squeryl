package org.squeryl.h2

import org.squeryl.test._

import org.squeryl.framework.DBConnector
import org.squeryl.adapters.H2Adapter

import org.squeryl.Session

trait H2_Connection extends DBConnector{
  def connectToDb() : Option[() => Session] = {
    if(config.hasProps("h2.connectionString", "h2.user", "h2.password")){
      Class.forName("org.h2.Driver")
      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("h2.connectionString"),
          config.getProp("h2.user"),
          config.getProp("h2.password")
        )
        c.setAutoCommit(false)
        Session.create(c, new H2Adapter)
      })
    }else{
      None
    }
  }
}

class H2_UuidTests extends UuidTests with H2_Connection
class H2_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with H2_Connection
class H2_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with H2_Connection
class H2_TransactionTests extends TransactionTests with H2_Connection
class H2_SchoolDb2 extends schooldb2.SchoolDb2Tests with H2_Connection
class H2_SchoolDb extends schooldb.SchoolDbTestRun with H2_Connection
class H2_TestCustomTypesMode extends customtypes.TestCustomTypesMode with H2_Connection
class H2_KickTheTires extends demo.KickTheTires with H2_Connection
class H2_MusicDb extends musicdb.MusicDbTestRun with H2_Connection
class H2_LeftJoinTest extends LeftJoinTest with H2_Connection
class H2_ConnectionClosing extends ConnectionClosingTest with H2_Connection {
  def dbSpecificSelectNow: String = "select now()"
}
