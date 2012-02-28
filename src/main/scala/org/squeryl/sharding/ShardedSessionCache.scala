package org.squeryl.sharding

import org.squeryl.Session

/**
 *
 * User: takeshita
 * Create: 12/02/15 19:18
 */

class ShardedSessionCache(){

  var shardedSessionRepository : ShardedSessionRepository = new ShardedSessionRepositoryImpl()


  protected val shardedSessions = new ThreadLocal[scala.collection.mutable.Map[String, ShardedSession]]{
    override def initialValue() = scala.collection.mutable.HashMap.empty

    override def remove() {
      get().values.foreach(entry => {
        entry.forceClose()
      })
    }
  }

  def getSession(name : String, mode : ShardMode.Value) : ShardedSession = {
    _getSession(name) match{
      case Some(entry) => {
        if(ShardMode.hasSameFunction(entry.shardMode, mode)){
          entry
        }else{
          entry.forceClose()
          _createSession(name,mode)
        }
      }
      case None => {
        _createSession(name,mode)
      }
    }
  }
  def removeSession(session : ShardedSession) = {
    session.forceClose()
    shardedSessions.get().remove(session.shardName)
  }

  protected def _createSession(name : String , mode : ShardMode.Value) = {
    val session = shardedSessionRepository(name,mode)
    shardedSessions.get() +=(name -> session)
    session
  }

  protected def _getSession( name : String) : Option[ShardedSession] = {
    shardedSessions.get().get(name)
  }

  protected def _setSession( name : String , mode : ShardMode.Value, session : Session) : ShardedSession = {
    val shardedSession = new ShardedSessionImpl(name,mode,session)
    shardedSessions.get().update(name,shardedSession)
    shardedSession
  }



}