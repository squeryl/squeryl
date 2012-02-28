package org.squeryl.mysql

import org.squeryl.sharding.{DatabaseConfig, SimpleShardingBuilderInitializer}
import org.squeryl.sharding.scenario.BlogDbTestRun


/**
 * To test
 * User: takeshita
 * Create: 12/02/15 20:21
 */

trait MySQL_ShardConnection extends SimpleShardingBuilderInitializer {


  def skipTest_? = {
    !(config.hasProps("mysql.connectionForShard1") &&
    config.hasProps("mysql.connectionForShard2") )
  }

  def shardSettings: List[(String, List[DatabaseConfig], List[DatabaseConfig])] = {
    List(
      ("mysql_shard1" ,List(new DatabaseConfig(config.getProp("mysql.connectionForShard1"),None,None )),Nil),
      ("mysql_shard2" ,List(new DatabaseConfig(config.getProp("mysql.connectionForShard2"),None,None) ),Nil)
    )
  }
}
class MySQL_Sharding_BlogDb extends BlogDbTestRun with MySQL_ShardConnection