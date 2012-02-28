package org.squeryl.sharding.builder

import org.squeryl.sharding.{ShardedSessionFactory, ShardedSessionRepository}

/**
 * Builder to create shard session
 * User: takeshita
 * Date: 11/09/07
 * Time: 23:49
 * To change this template use File | Settings | File Templates.
 */

trait ShardedSessionBuilder{
  var name : String = "default"

  def create() : ShardedSessionFactory = create(this.name)
  def create( _name : String) : ShardedSessionFactory
}