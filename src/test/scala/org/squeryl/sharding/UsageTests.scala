package org.squeryl.sharding

import builder.SimpleShardedSessionBuilder
import org.scalatest.FlatSpec
import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import org.squeryl.PrimitiveTypeMode

/**
 *
 * User: takeshita
 * Create: 12/01/21 19:40
 */

class SimpleUsageTest extends FlatSpec with MustMatchers {

  "Sharded sessions" should "be initialized as such." in{
    // first shard
    {
      val builder = new SimpleShardedSessionBuilder()
      builder.name = "FirstShard"
      // only writer
      builder.addWriter("jdbc:h2:mem:shard1")

      //register shard
      ShardedSession.addFactory(builder.create)
      //PrimitiveTypeMode.shardedSessionCache.shardedSessionRepository.addFactory(builder.create())
    }
    //second shard
    {
      // In service, we often use Master/Slave database.
      // So you can init master db as writer and slave dbs as readers

      val builder = new SimpleShardedSessionBuilder()
      builder.name = "SecondShard"
      builder.addWriter(new DatabaseConfig("jdbc:h2:mem:shard2"))
      builder.addReader(new DatabaseConfig("jdbc:h2:mem:shard2_slave1"))
      builder.addReader(new DatabaseConfig("jdbc:h2:mem:shard2_slave2"))

      //register shard
      ShardedSession.addFactory(builder.create)
      //PrimitiveTypeMode.shardedSessionCache.shardedSessionRepository.addFactory(builder.create())
    }


  }

}
