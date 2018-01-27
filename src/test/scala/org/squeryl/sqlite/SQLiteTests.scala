package org.squeryl.sqlite

import org.squeryl.test._

import org.squeryl.framework.DBConnector
import org.squeryl.adapters.SQLiteAdapter

import org.squeryl.{AbstractSession, Session}
import java.sql.Connection

/*
 * To run on command line :
 *
 * org.scalatest.tools.Runner -s org.squeryl.sqlite.SQLite_SchoolDb -eNDXEHLOW
 *
 * org.scalatest.tools.Runner -s org.squeryl.sqlite.SQLite_SchoolDb -eNDXEHLOW -n SingleTestRun
 *
 * */

trait SQLite_ConnectionCommon extends DBConnector {
  def connectToDbCommon(sessionFunc: Connection => AbstractSession): Option[() => AbstractSession] = {
    if (config.hasProps("sqlite.connectionString")) {
      Class.forName("org.sqlite.JDBC")
      Some(() => {
        val c = java.sql.DriverManager.getConnection(config.getProp("sqlite.connectionString"))
        val s = sessionFunc(c)
        s
      })
    } else {
      None
    }
  }
}

trait SQLite_Connection extends DBConnector with SQLite_ConnectionCommon {
  def sessionCreator(): Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new SQLiteAdapter))
}

trait SQLite_LazyConnection extends DBConnector with SQLite_ConnectionCommon {
  def sessionCreator(): Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new SQLiteAdapter))

}

/*
 * Non-Lazy
 */
class SQLite_UuidTests extends UuidTests with SQLite_Connection
class SQLite_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with SQLite_Connection
class SQLite_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with SQLite_Connection
class SQLite_TransactionTests extends TransactionTests with SQLite_Connection
class SQLite_SchoolDb2 extends schooldb2.SchoolDb2Tests with SQLite_Connection
class SQLite_SchoolDb extends schooldb.SchoolDbTestRun with SQLite_Connection {
  override val ignoredTests = List(
    "OptimisticCC1"
  )
}
class SQLite_TestCustomTypesMode extends customtypes.TestCustomTypesMode with SQLite_Connection
class SQLite_KickTheTires extends demo.KickTheTires with SQLite_Connection {
  // This test generates a sentence with double parentheses after `in` clause
  // In SQLite this causes that the query returns only one item
  override val ignoredTests = List(
    "kick tires"
  )
}
class SQLite_MusicDb extends musicdb.MusicDbTestRun with SQLite_Connection {
  // Regexp not supported
  override val ignoredTests = List(
    "UpperAndLowerFuncs"
  )
}
class SQLite_LeftJoinTest extends LeftJoinTest with SQLite_Connection
class SQLite_ConnectionClosing extends ConnectionClosingTest with SQLite_Connection {
  def dbSpecificSelectNow: String = "select CURRENT_TIMESTAMP"
}
class SQLite_LogicalBooleanObjTests extends LogicalBooleanObjTests with SQLite_Connection

class SQLite_CommonTableExpressions extends schooldb.CommonTableExpressions with SQLite_Connection

/*
 * Lazy
 */
class SQLite_LazyUuidTests extends UuidTests with SQLite_LazyConnection
class SQLite_LazyNestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with SQLite_LazyConnection
class SQLite_LazySchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with SQLite_LazyConnection
class SQLite_LazyTransactionTests extends TransactionTests with SQLite_LazyConnection
class SQLite_LazySchoolDb2 extends schooldb2.SchoolDb2Tests with SQLite_LazyConnection
class SQLite_LazySchoolDb extends schooldb.SchoolDbTestRun with SQLite_LazyConnection {
  override val ignoredTests = List(
    "OptimisticCC1"
  )
}
class SQLite_LazyTestCustomTypesMode extends customtypes.TestCustomTypesMode with SQLite_LazyConnection
class SQLite_LazyKickTheTires extends demo.KickTheTires with SQLite_LazyConnection {
  // This test generates a sentence with double parentheses after `in` clause
  // In SQLite this causes that the query returns only one item
  override val ignoredTests = List(
    "kick tires"
  )
}
class SQLite_LazyMusicDb extends musicdb.MusicDbTestRun with SQLite_LazyConnection {
  // Regexp not supported
  override val ignoredTests = List(
    "UpperAndLowerFuncs"
  )
}
class SQLite_LazyLeftJoinTest extends LeftJoinTest with SQLite_LazyConnection
class SQLite_LazyConnectionClosing extends ConnectionClosingTest with SQLite_LazyConnection {
  def dbSpecificSelectNow: String = "select CURRENT_TIMESTAMP"
}
class SQLite_LazyLogicalBooleanObjTests extends LogicalBooleanObjTests with SQLite_LazyConnection
