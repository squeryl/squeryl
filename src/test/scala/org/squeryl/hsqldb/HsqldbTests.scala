package org.squeryl.hsqldb

import org.squeryl.framework.DBConnector
import java.sql.Connection
import org.squeryl.{Session, AbstractSession}
import org.squeryl.adapters.HsqldbAdapter
import org.squeryl.test._
import scala.Some
import org.squeryl.test.arrays.PrimitiveArrayTest

trait Hsqldb_ConnectionCommon extends DBConnector {
  def connectToDbCommon(sessionFunc: Connection => AbstractSession) : Option[() => AbstractSession] = {
    if(config.hasProps("hsqldb.connectionString", "hsqldb.user", "hsqldb.password")){
      Class.forName("org.hsqldb.jdbc.JDBCDriver")
      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("hsqldb.connectionString"),
          config.getProp("hsqldb.user"),
          config.getProp("hsqldb.password")
        )
        sessionFunc(c)
      })
    }else{
      None
    }
  }
}

trait Hsqldb_Connection extends DBConnector with Hsqldb_ConnectionCommon {
  def connectToDb() : Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new HsqldbAdapter))
}


trait Hsqldb_LazyConnection extends DBConnector with Hsqldb_ConnectionCommon {
  def connectToDb() : Option[() => AbstractSession] = connectToDbCommon(Session.create(_, new HsqldbAdapter))

}

/*
 * Non-Lazy
 */
class Hsqldb_UuidTests extends UuidTests with Hsqldb_Connection
class Hsqldb_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with Hsqldb_Connection
class Hsqldb_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with Hsqldb_Connection
class Hsqldb_TransactionTests extends TransactionTests with Hsqldb_Connection
class Hsqldb_SchoolDb2 extends schooldb2.SchoolDb2Tests with Hsqldb_Connection
class Hsqldb_SchoolDb extends schooldb.SchoolDbTestRun with Hsqldb_Connection {
  override val ignoredTests: List[String] = List("assertColumnNameChangeWithDeclareSyntax")
}
class Hsqldb_TestCustomTypesMode extends customtypes.TestCustomTypesMode with Hsqldb_Connection
class Hsqldb_KickTheTires extends demo.KickTheTires with Hsqldb_Connection
class Hsqldb_MusicDb extends musicdb.MusicDbTestRun with Hsqldb_Connection
class Hsqldb_LeftJoinTest extends LeftJoinTest with Hsqldb_Connection
class Hsqldb_ConnectionClosing extends ConnectionClosingTest with Hsqldb_Connection {
  def dbSpecificSelectNow: String = "VALUES(CURRENT_TIMESTAMP)"
}
class Hsqldb_LogicalBooleanObjTests extends LogicalBooleanObjTests with Hsqldb_Connection
class Hsqldb_ArrayTests extends PrimitiveArrayTest with Hsqldb_Connection



/*
 * Lazy
 */
class Hsqldb_LazyUuidTests extends UuidTests with Hsqldb_LazyConnection
class Hsqldb_LazyNestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with Hsqldb_LazyConnection
class Hsqldb_LazySchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with Hsqldb_LazyConnection
class Hsqldb_LazyTransactionTests extends TransactionTests with Hsqldb_LazyConnection
class Hsqldb_LazySchoolDb2 extends schooldb2.SchoolDb2Tests with Hsqldb_LazyConnection
class Hsqldb_LazySchoolDb extends schooldb.SchoolDbTestRun with Hsqldb_LazyConnection {
  override val ignoredTests: List[String] = List("assertColumnNameChangeWithDeclareSyntax")
}
class Hsqldb_LazyTestCustomTypesMode extends customtypes.TestCustomTypesMode with Hsqldb_LazyConnection
class Hsqldb_LazyKickTheTires extends demo.KickTheTires with Hsqldb_LazyConnection
class Hsqldb_LazyMusicDb extends musicdb.MusicDbTestRun with Hsqldb_LazyConnection
class Hsqldb_LazyLeftJoinTest extends LeftJoinTest with Hsqldb_LazyConnection
class Hsqldb_LazyConnectionClosing extends ConnectionClosingTest with Hsqldb_LazyConnection {
  def dbSpecificSelectNow: String = "VALUES(CURRENT_TIMESTAMP)"
}
class Hsqldb_LazyLogicalBooleanObjTests extends LogicalBooleanObjTests with Hsqldb_LazyConnection
class Hsqldb_LazyArrayTests extends PrimitiveArrayTest with Hsqldb_LazyConnection

