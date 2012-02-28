package org.squeryl.sharding.builder

import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters.MySQLInnoDBAdapter
import org.squeryl.SquerylException
import org.squeryl.sharding._
import org.squeryl.logging.StatisticsListener

/**
 * This builder class offers the way of brief initialization for sharding.
 * User: takeshita
 * Date: 11/09/07
 * Time: 23:38
 * To change this template use File | Settings | File Templates.
 */

class SimpleShardedSessionBuilder extends ShardedSessionBuilder{

  var adapter : DatabaseAdapter = new MySQLInnoDBAdapter

  var connectionManager : ConnectionManager = new AlwaysCreateConnectionManager()

  protected var readerConfigs : List[DatabaseConfig] = Nil
  protected var writerConfigs : List[DatabaseConfig] = Nil

  /**
   * Set appropriate adapter detected from JDBC url
   */
  var autoDetectAdapter = true
  /**
   * Load Driver class automatically
   */
  var autoLoadDriverClass = true

  /**
   * Use for detecting adapter and driver class
   */
  var adapterSelector = AdapterSelector


  var enableConsoleStatisticsListener = false

  var statisticsListener : Option[() => StatisticsListener] = None


  def addReader(config : DatabaseConfig) : SimpleShardedSessionBuilder = {
    readerConfigs = readerConfigs :+ config
    this
  }

  def addReader(url : String , username : String , password : String) : SimpleShardedSessionBuilder = {
    readerConfigs = readerConfigs :+ new DatabaseConfig(url,Some(username),Some(password))
    this
  }
  def addReader(url : String) : SimpleShardedSessionBuilder = {
    readerConfigs = readerConfigs :+ new DatabaseConfig(url)
    this
  }
  def addWriter(config : DatabaseConfig) : SimpleShardedSessionBuilder = {
    writerConfigs = writerConfigs :+ config
    this
  }
  def addWriter(url : String , username : String , password : String) : SimpleShardedSessionBuilder = {
    writerConfigs = writerConfigs :+ new DatabaseConfig(url,Some(username),Some(password))
    this
  }
  def addWriter(url : String) : SimpleShardedSessionBuilder = {
    writerConfigs = writerConfigs :+ new DatabaseConfig(url)
    this
  }

  def addBoth(config : DatabaseConfig) = {
    addReader(config)
    addWriter(config)
    this
  }

  def create(_name: String) = {
    if(writerConfigs.isEmpty){
      throw new NoDatabaseConfigException()
    }

    val adapter = if(autoDetectAdapter){
      adapterSelector(writerConfigs(0).url) match{
        case Some(adap) => adap
        case None => throw new SquerylException("Can't detect appropriate adapter for url %s".format(
          writerConfigs(0).url
        ))
      }
    }else this.adapter

    val session = new SimpleShardedSession(
      _name,
      connectionManager,
      adapter
    )
    writerConfigs.foreach(c => {
      session.addConfig(ShardMode.Write,c)
    })
    // if there is no reader configs,use writer configs instead.
    if(readerConfigs.isEmpty){
      writerConfigs.foreach(c => {
        session.addConfig(ShardMode.Read,c)
      })
    }else{
      readerConfigs.foreach(c => {
        session.addConfig(ShardMode.Read,c)
      })
    }

    // Load driver class
    if(autoLoadDriverClass){
      val driverClassName = adapterSelector.getDriverClassName(adapter)
      if(driverClassName == null){
        throw new SquerylException("Can't detect driver class. Adapter:" + adapter.getClass.getName)
      }
      Class.forName(driverClassName)
    }

    // set statistics listener
    if(statisticsListener.isDefined){
      session.statisticsListener = statisticsListener
    }else if(enableConsoleStatisticsListener){
      session.statisticsListener = Some( () => ConsoleStatisticsListener)
    }

    session
  }
}

class NoDatabaseConfigException extends SquerylException("There is no database config." +
  "At lease one writer config is needed"){

}
