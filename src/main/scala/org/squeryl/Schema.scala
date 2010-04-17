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
package org.squeryl


import internals.{FieldMetaData, DatabaseAdapter, StatementWriter}
import reflect.{Manifest}
import scala.collection.mutable.ArrayBuffer
import java.sql.SQLException;


trait Schema {

  private val _tables = new ArrayBuffer[Table[_]] 

  private def _dbAdapter = Session.currentSession.databaseAdapter

  object NamingConventionTransforms {
    
    def camelCase2underScore(name: String) =
      name.toList.map(c => if(c.isUpper) "_" + c else c).mkString
  }

  def columnNameFromPropertyName(propertyName: String) = propertyName

  def tableNameFromClassName(tableName: String) = tableName

  def printDml = {

    for(t <- _tables) {
      val sw = new StatementWriter(true, _dbAdapter)
      _dbAdapter.writeCreateTable(t, sw, this)
      println(sw.statement)
    }
  }

  /**
   * This will drop all tables and related sequences in the schema... it's a
   * dangerous operation, typically this is only usefull for devellopment
   * database instances, the method is protected in order to make it a little
   * less 'accessible'  
   */
  protected def drop(failOnNonExistingTable: Boolean):Unit = {

    for(t <- _tables) {

      val s = Session.currentSession.connection.createStatement
      try {
        s.execute("drop table " + t.name)
        _dbAdapter.postDropTable(t)
      }
      catch {
        case e:SQLException => if(failOnNonExistingTable) throw e
      }
    }
  }

  protected def drop:Unit = drop(false)

  def create = {

    for(t <- _tables) {
      var sw:StatementWriter = null
      try {
        sw = new StatementWriter(_dbAdapter)
        _dbAdapter.writeCreateTable(t, sw, this)
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

  protected def columnTypeFor(fieldMetaData: FieldMetaData, databaseAdapter: DatabaseAdapter): String =
    databaseAdapter.databaseTypeFor(fieldMetaData)

  private [squeryl] def _columnTypeFor(fmd: FieldMetaData, dba: DatabaseAdapter): String =
    this.columnTypeFor(fmd, dba)
  
  protected def tableNameFromClass(c: Class[_]):String =     
    c.getSimpleName

  protected def table[T]()(implicit manifestT: Manifest[T]): Table[T] =
    table(tableNameFromClass(manifestT.erasure))(manifestT)
  
  protected def table[T](name: String)(implicit manifestT: Manifest[T]): Table[T] = {
    val t = new Table[T](name, manifestT.erasure.asInstanceOf[Class[T]], this)
    _tables.append(t)
    t
  }

  protected def view[T]()(implicit manifestT: Manifest[T]): View[T] =
    view(tableNameFromClass(manifestT.erasure))(manifestT)

  protected def view[T](name: String)(implicit manifestT: Manifest[T]): View[T] =
    new View[T](name)(manifestT)
}
