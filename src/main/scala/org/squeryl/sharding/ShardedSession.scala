package org.squeryl.sharding

import java.sql.Connection
import org.squeryl.{PrimitiveTypeMode, SquerylException, Session}

/**
 * Created by IntelliJ IDEA.
 * User: takezoux3
 * Date: 12/02/06
 * Time: 23:56
 * To change this template use File | Settings | File Templates.
 */
object ShardedSession{
  def addFactory( sessionFactory : ShardedSessionFactory) = {
    PrimitiveTypeMode.shardedSessionCache.shardedSessionRepository.addFactory(sessionFactory)
  }
}


trait ShardedSession{
  def shardName : String
  def shardMode : ShardMode.Value

  def bindToCurrentThread() : Unit
  def unbindFromCurrentThread() : Unit

  def use() : Unit
  def cleanup() : Unit

  /**
   * try to close.
   * If connection is closed,it returns true
   * @return true if connection is closed
   */
  def safeClose() : Boolean

  /**
   * force close.
   * If connection is closed,it returns true
   * @return always true
   */
  def forceClose() : Boolean
  def beginTransaction() : Unit
  def commitTransaction() : Boolean
  def rollback() : Boolean

  def connection : Connection

}

case class ShardedSessionImpl(shardName : String ,shardMode : ShardMode.Value,session : Session) extends ShardedSession{

  private var useCounter = 0
  private var closed = false

  def bindToCurrentThread() {session.bindToCurrentThread}

  def unbindFromCurrentThread() {session.unbindFromCurrentThread}

  def use() {
    if(closed){
      throw new SquerylException("ShededSession for (%s,%s) is already closed!".format(shardName,shardMode))
    }
    useCounter += 1
  }


  def cleanup() {session.cleanup}

  def safeClose() : Boolean = {
    if(closed) return false
    useCounter -= 1
    if(useCounter <= 0){
      session.close
      closed = true
      useCounter = 0
      true
    }else{
      false
    }
  }


  def forceClose()  : Boolean = {
    if(closed)return false
    session.close
    useCounter = 0
    closed = true
    true
  }
  
  private var transactionCounter = 0
  
  def beginTransaction() = {
    if(transactionCounter == 0){
      val c = session.connection
      if(c.getAutoCommit()){
        c.setAutoCommit(false)
      }
    }
    transactionCounter += 1
  }
  
  def commitTransaction() = {
    if(transactionCounter > 0){
      transactionCounter -= 1
      if(transactionCounter <= 0){
        transactionCounter = 0
        val c = session.connection
        c.commit
        true
      }else false
    }else{
      false
    }
  }
  
  def rollback() = {
    if(transactionCounter > 0){
      session.connection.rollback()
      transactionCounter = 0
      true
    }else{
      false
    }
  }

  def connection: Connection = session.connection
}

object ShardMode extends Enumeration{

  val Read = Value(0,"read")
  val Write = Value(1,"write")

  def hasSameFunction( val1 : ShardMode.Value , val2: ShardMode.Value) = {
    val1.id >= val2.id
  }
}
