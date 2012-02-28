package org.squeryl.sharding

import java.sql.Connection

/**
 *
 * User: takeshita
 * Create: 12/02/15 14:34
 */

class DummyShardedSession(val shardName : String, val shardMode : ShardMode.Value) extends ShardedSession {

  def bindToCurrentThread() {}

  def unbindFromCurrentThread() {}

  def use() {}

  def cleanup() {}

  def safeClose(): Boolean = true

  def forceClose(): Boolean = true

  def beginTransaction() {}

  def commitTransaction() = {true}

  def rollback() = {true}

  def connection: Connection = null
}