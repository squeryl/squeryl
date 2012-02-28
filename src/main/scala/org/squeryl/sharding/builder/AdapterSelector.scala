package org.squeryl.sharding.builder

import org.squeryl.internals.DatabaseAdapter
import org.squeryl.SquerylException
import org.squeryl.adapters._

/**
 * Select appropriate adapter from url
 * User: takeshita
 * Date: 11/09/07
 * Time: 23:56
 * To change this template use File | Settings | File Templates.
 */

object AdapterSelector extends AdapterSelector

class AdapterSelector{

  /**
   *
   * @param url jdbc url
   * @return Option[DatabaseAdapter]
   */
  def apply(url : String) : Option[DatabaseAdapter] = {
    if(url == null) return None
    val splits = url.split(":")
    if(splits.length < 2){
      return None
    }
    if(splits(0) != "jdbc"){
      return None
    }

    splits(1).toLowerCase match{
      case "mysql" => Some(new MySQLInnoDBAdapter)
      case "h2" => Some(new H2Adapter())
      case "postgresql" => Some(new PostgreSqlAdapter())
      case "oracle" => Some(new OracleAdapter())
      case "db2" => Some(new DB2Adapter())
      case "derby" => Some(new DerbyAdapter())
      case "microsoft" => Some(new MSSQLServer())
      case _ => None
    }

  }

  /**
   * get JDBC driver's class name from DatabaseAdapter
   */
  def getDriverClassName(databaseAdapter : DatabaseAdapter) : String = {
    databaseAdapter match{
      case a : MySQLInnoDBAdapter => "com.mysql.jdbc.Driver"
      case a : MySQLAdapter => "com.mysql.jdbc.Driver"
      case a : H2Adapter => "org.h2.Driver"
      case a : PostgreSqlAdapter => "org.postgresql.Driver"
      case a : OracleAdapter => "oracle.jdbc.driver.OracleDriver"
      case a : DB2Adapter => "com.ibm.db2.jcc.DB2Driver"
      case a : DerbyAdapter => "org.apache.derby.jdbc.EmbeddedDriver"
      case a : MSSQLServer => "com.microsoft.jdbc.sqlserver.SQLServerDriver"
      case _ => null
    }
  }

}