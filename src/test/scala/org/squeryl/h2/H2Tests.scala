package org.squeryl.h2

import org.squeryl.test._

import org.squeryl.framework.DBConnector
import org.squeryl.adapters.H2Adapter

import org.squeryl.{AbstractSession, Session}
import java.sql.Connection

/*
 * To run on command line : 
 * 
 * org.scalatest.tools.Runner -s org.squeryl.h2.H2_SchoolDb -eNDXEHLOW
 * 
 * org.scalatest.tools.Runner -s org.squeryl.h2.H2_SchoolDb -eNDXEHLOW -n SingleTestRun
 * 
 * */

trait H2_ConnectionCommon extends DBConnector {
  def connectToDbCommon(sessionFunc: Connection => AbstractSession) : Option[() => AbstractSession] = {
    if(config.hasProps("h2.connectionString", "h2.user", "h2.password")){
      Class.forName("org.h2.Driver")
      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("h2.connectionString"),
          config.getProp("h2.user"),
          config.getProp("h2.password")
        )
        sessionFunc(c)
      })
    }else{
      None
    }
  }
}

trait H2_Connection extends DBConnector with H2_ConnectionCommon {
  def connectToDb() : Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new H2Adapter))
}


trait H2_LazyConnection extends DBConnector with H2_ConnectionCommon {
  def connectToDb() : Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new H2Adapter))

}

/*
 * Non-Lazy
 */
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
class H2_LogicalBooleanObjTests extends LogicalBooleanObjTests with H2_Connection



/*
 * Lazy
 */
class H2_LazyUuidTests extends UuidTests with H2_LazyConnection
class H2_LazyNestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with H2_LazyConnection
class H2_LazySchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with H2_LazyConnection
class H2_LazyTransactionTests extends TransactionTests with H2_LazyConnection
class H2_LazySchoolDb2 extends schooldb2.SchoolDb2Tests with H2_LazyConnection
class H2_LazySchoolDb extends schooldb.SchoolDbTestRun with H2_LazyConnection
class H2_LazyTestCustomTypesMode extends customtypes.TestCustomTypesMode with H2_LazyConnection
class H2_LazyKickTheTires extends demo.KickTheTires with H2_LazyConnection
class H2_LazyMusicDb extends musicdb.MusicDbTestRun with H2_LazyConnection
class H2_LazyLeftJoinTest extends LeftJoinTest with H2_LazyConnection
class H2_LazyConnectionClosing extends ConnectionClosingTest with H2_LazyConnection {
  def dbSpecificSelectNow: String = "select now()"
}
class H2_LazyLogicalBooleanObjTests extends LogicalBooleanObjTests with H2_LazyConnection

