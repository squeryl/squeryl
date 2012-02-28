package org.squeryl.h2

import org.squeryl.sharding.scenario.BlogDbTestRun
import org.squeryl.sharding.builder.{SimpleShardedSessionBuilder, ShardedSessionBuilder}
import org.squeryl.sharding.{ConnectionManager, ShardedSessionFactory, DatabaseConfig, SimpleShardingBuilderInitializer}
import java.sql.{DriverManager, Connection}


/**
 *
 * User: takeshita
 * Create: 12/01/22 22:57
 */

trait H2_FileShards extends SimpleShardingBuilderInitializer{


  def skipTest_? = false

  def shardSettings: List[(String, List[DatabaseConfig], List[DatabaseConfig])] = {
    List(
     ("h2_file_shard1" ,List(new DatabaseConfig("jdbc:h2:file:target/h2db/shard1",None,None )),Nil),
     ("h2_file_shard2" ,List(new DatabaseConfig("jdbc:h2:file:target/h2db/shard2",None,None) ),Nil)
    )
  }

}


class H2_FileShards_BlogDb extends BlogDbTestRun with H2_FileShards