package org.squeryl.sharding

import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import org.scalatest.{FlatSpec, FunSuite}
import org.jmock.Expectations._
import org.junit.runner.RunWith
import org.scalatest.junit.{JUnitRunner, MustMatchersForJUnit}
import org.scalatest.mock.{JMockExpectations, JMockCycle}


/**
 * Created by IntelliJ IDEA.
 * User: takezoux3
 * Date: 12/01/11
 * Time: 18:02
 * To change this template use File | Settings | File Templates.
 */

@RunWith(classOf[JUnitRunner])
class ShardingDslTest extends FlatSpec with MustMatchersForJUnit {

  val cycle = new JMockCycle
  import cycle._

  val session = mock[ShardedSession]
  val shardedSessionRepo = mock[ShardedSessionRepository]

  val dsl = new ShardingDsl {
    override val shardedSessionCache = new ShardedSessionCache{
      shardedSessionRepository = shardedSessionRepo
    }
  }

  def mustThrowException( func : => Unit) = {
    try{
      func
      fail()
    }catch{
      case e : MyDummyException =>
    }
  }

  def useExecutions(e : JMockExpectations) {
    import e._
    oneOf(session).use()
    oneOf(session).bindToCurrentThread()
    oneOf(session).unbindFromCurrentThread()
    oneOf(session).cleanup()
    oneOf(session).safeClose();will(returnValue(true))
    oneOf(session).forceClose();will(returnValue(false))
  }

  def transactionExecutions(e : JMockExpectations) {
    import e._

    oneOf(session).use()
    oneOf(session).beginTransaction()
    oneOf(session).bindToCurrentThread()
    oneOf(session).unbindFromCurrentThread()
    oneOf(session).cleanup()
    oneOf(session).commitTransaction()
    oneOf(session).safeClose();will(returnValue(true))
    oneOf(session).forceClose();will(returnValue(false))
  }

  "use" should "execute query in write mode without transaction" in{
    val shardName = "shard1"

    expecting{ e => import e._
      oneOf(shardedSessionRepo).apply(shardName,ShardMode.Write);will(returnValue(session))
      allowing(session).shardMode;will(returnValue(ShardMode.Write))
      allowing(session).shardName;will(returnValue(shardName))
    useExecutions(e)
    }

    val v = dsl.use(shardName){
      doDatabaseAccess()
      "returned value"
    }
    v must be("returned value")
  }


  "read" should "execute query in read mode without transaction" in{
    val shardName = "shard1"

    expecting{ e => import e._
    oneOf(shardedSessionRepo).apply(shardName,ShardMode.Read);will(returnValue(session))
    allowing(session).shardMode;will(returnValue(ShardMode.Read))
    allowing(session).shardName;will(returnValue(shardName))
    useExecutions(e)
    }

    val v = dsl.read(shardName){
      doDatabaseAccess()
      "returned value"
    }
    v must be("returned value")
  }

  "write" should "execute query in read mode with transaction" in{
    val shardName = "shard1"

    expecting{ e => import e._
    oneOf(shardedSessionRepo).apply(shardName,ShardMode.Write);will(returnValue(session))
    allowing(session).shardMode;will(returnValue(ShardMode.Write))
    allowing(session).shardName;will(returnValue(shardName))
    transactionExecutions(e)
    }

    val v = dsl.write(shardName){
      doDatabaseAccess()
      "returned value"
    }
    v must be("returned value")
  }

  "write" should "rollback if exception occurs" in{

    val shardName = "shard1"

    expecting{ e => import e._
    oneOf(shardedSessionRepo).apply(shardName,ShardMode.Write);will(returnValue(session))
    allowing(session).shardMode;will(returnValue(ShardMode.Write))
    allowing(session).shardName;will(returnValue(shardName))

    oneOf(session).use()
    oneOf(session).beginTransaction()
    oneOf(session).bindToCurrentThread()
    oneOf(session).unbindFromCurrentThread()
    oneOf(session).cleanup()
    oneOf(session).rollback();will(returnValue(true))
    atLeast(1).of(session).forceClose();will(returnValue(false))
    }

    mustThrowException{
      dsl.write(shardName){
        throw new MyDummyException
        "returned value"
      }
    }
  }

  def dummySession(shardName : String, mode : ShardMode.Value) = {
    new DummyShardedSession(shardName,mode)
  }
  
  "nested shard call" should "create session once for each shard/mode" in{

    val shardName = "shard1"
    val shardName2 = "shard2"
    val shardName3 = "shard3"

    expecting{ e => import e._
    oneOf(shardedSessionRepo).apply(shardName,ShardMode.Write);will(returnValue(dummySession(shardName,ShardMode.Write)))
    oneOf(shardedSessionRepo).apply(shardName2,ShardMode.Read);will(returnValue(dummySession(shardName2,ShardMode.Read)))
    oneOf(shardedSessionRepo).apply(shardName2,ShardMode.Write);will(returnValue(dummySession(shardName2,ShardMode.Write)))
    oneOf(shardedSessionRepo).apply(shardName3,ShardMode.Read);will(returnValue(dummySession(shardName3,ShardMode.Read)))
    }
    
    dsl.use(shardName){
      dsl.read(shardName2){
        dsl.use(shardName){
          dsl.read(shardName){
            dsl.write(shardName2){
              dsl.read(shardName3){
                doDatabaseAccess()
              }
            }
          }
        }
      }
    }


  }

  def doDatabaseAccess() {/* dummy method */}
}

class MyDummyException extends Exception