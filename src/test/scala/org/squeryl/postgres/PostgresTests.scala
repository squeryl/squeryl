package org.squeryl.postgres


import org.squeryl.test._
import org.squeryl.framework.DBConnector
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.Session
import org.squeryl.test.arrays.PrimitiveArrayTest

trait Postgresql_Connection extends DBConnector{
  def connectToDb() : Option[() => Session] = {
    if(config.hasProps("postgresql.connectionString", "postgresql.user", "postgresql.password")){
      Class.forName("org.postgresql.Driver")

      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("postgresql.connectionString"),
          config.getProp("postgresql.user"),
          config.getProp("postgresql.password")
        )
        c.setAutoCommit(false)
        Session.create(c, new PostgreSqlAdapter)
      })
    }else{
      None
    }
  }
}

class Postgresql_ArrayTests extends PrimitiveArrayTest with Postgresql_Connection
class Postgresql_UuidTests extends UuidTests with Postgresql_Connection
class Postgresql_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with Postgresql_Connection
class Postgresql_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with Postgresql_Connection
class Postgresql_TransactionTests extends TransactionTests with Postgresql_Connection
class Postgresql_SchoolDb2 extends schooldb2.SchoolDb2Tests with Postgresql_Connection
class Postgresql_SchoolDb extends schooldb.SchoolDbTestRun with Postgresql_Connection{
  override val ignoredTests = List("blobTest", "OuterJoinMixed1", "assertColumnNameChangeWithDeclareSyntax")
}
//class Postgresql_TestCustomTypesMode extends customtypes.TestCustomTypesMode with Postgresql_Connection
class Postgresql_KickTheTires extends demo.KickTheTires with Postgresql_Connection
class Postgresql_MusicDb extends musicdb.MusicDbTestRun with Postgresql_Connection
class Postgresql_LeftJoinTest extends LeftJoinTest with Postgresql_Connection
class Postgresql_ConnectionClosing extends ConnectionClosingTest with Postgresql_Connection {
  def dbSpecificSelectNow: String = "select now()"
}



