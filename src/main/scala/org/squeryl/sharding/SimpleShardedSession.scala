package org.squeryl.sharding

import org.squeryl.internals.DatabaseAdapter
import org.squeryl.SquerylException
import util.Random
/**
 * Simple sharding session
 * User: takezou
 * Date: 11/09/04
 * Time: 21:15
 * To change this template use File | Settings | File Templates.
 */

class SimpleShardedSession(val shardName : String,
                            val connectionManager : ConnectionManager,
                            val databaseAdapter : DatabaseAdapter) extends ShardedSessionFactory {

  protected var readerConfigs : List[DatabaseConfig] = Nil
  protected var writerConfigs : List[DatabaseConfig] = Nil
  val random = new Random
  def selectWriter = {
    writer(random.nextInt(writerConfigs.size))
  }

  def selectReader = {
    reader(random.nextInt(readerConfigs.size))
  }


  def config(mode: ShardMode.Value, index: Int) = {
    mode match{
      case ShardMode.Read => {
        readerConfigs(index)
      }
      case ShardMode.Write => {
        writerConfigs(index)
      }
    }
    
  }
  
  
  def addConfig(mode : ShardMode.Value, config : DatabaseConfig) = {
    mode match{
      case ShardMode.Read => {
        readerConfigs = readerConfigs :+ config
      }
      case ShardMode.Write => {
        writerConfigs = writerConfigs :+ config
      }
    }
  }

}