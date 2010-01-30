package org.squeryl


import internals.StatementWriter
import reflect.{Manifest}
import scala.collection.mutable.ArrayBuffer
import java.sql.SQLException;


trait Schema {

  private val _tables = new ArrayBuffer[Table[_]] 

  private def _dbAdapter = Session.currentSession.databaseAdapter

  def printDml = {

    for(t <- _tables) {
      val sw = new StatementWriter(true, _dbAdapter)
      _dbAdapter.writeCreateTable(t, sw)
      println(sw.statement)
    }
  }

  /**
   * This will drop all tables and related sequences in the schema... it's a
   * dangerous operation, typically this is only usefull for devellopment
   * database instances, the method is protected in order to make it a little
   * less 'accessible'  
   */
  protected def drop = {

    for(t <- _tables) {

      val s = Session.currentSession.connection.createStatement
      s.execute("drop table " + t.name)
      _dbAdapter.postDropTable(t)
    }
  }

  def create = {

    for(t <- _tables) {
      var sw:StatementWriter = null
      try {
        sw = new StatementWriter(_dbAdapter)
        _dbAdapter.writeCreateTable(t, sw)
        val s = Session.currentSession.connection.createStatement
        s.execute(sw.statement)
      }
      catch {
        case e:SQLException => throw new RuntimeException("error creating table :\n" + sw.statement ,e)
      }
      try {
        _dbAdapter.postCreateTable(Session.currentSession, t)
      }
      catch {
        case e:SQLException => throw new RuntimeException(e)
      }
    }
  }
  
  protected def tableNameFromClass(c: Class[_]):String =     
    c.getSimpleName

  protected def table[T]()(implicit manifestT: Manifest[T]): Table[T] =
    table(tableNameFromClass(manifestT.erasure))(manifestT)
  
  protected def table[T](name: String)(implicit manifestT: Manifest[T]): Table[T] = {
    val t = new Table[T](name)(manifestT)
    _tables.append(t)
    t
  }

  protected def view[T]()(implicit manifestT: Manifest[T]): View[T] =
    view(tableNameFromClass(manifestT.erasure))(manifestT)

  protected def view[T](name: String)(implicit manifestT: Manifest[T]): View[T] =
    new View[T](name)(manifestT)  
}
