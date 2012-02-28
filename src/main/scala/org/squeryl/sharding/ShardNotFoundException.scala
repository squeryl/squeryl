package org.squeryl.sharding

import org.squeryl.SquerylException

/**
 *
 * User: takeshita
 * Create: 12/02/15 19:42
 */

case class ShardNotFoundException(shardName : String) extends SquerylException("Shard:%s is not found".format(shardName)){

}