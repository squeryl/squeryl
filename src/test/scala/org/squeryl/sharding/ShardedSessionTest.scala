package org.squeryl.sharding

import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.JMockCycle
import java.sql.Connection
import org.squeryl.adapters.H2Adapter
import org.squeryl.{SquerylException, Session}
import org.jmock.Expectations._

/**
 *
 * User: takeshita
 * Create: 12/02/15 16:47
 */

@RunWith(classOf[JUnitRunner])
class ShardedSessionTest extends FlatSpec with MustMatchers {

  val cycle = new JMockCycle
  import cycle._
  val connection = mock[Connection]

  def session(shardName : String = "shard1", mode : ShardMode.Value = ShardMode.Write) = {
    new ShardedSessionImpl(shardName,mode,new Session(connection,new H2Adapter,None))
  }
  def mustThrowError( func : => Unit) = {
    try{
      func
      fail()
    }catch{
      case e : SquerylException => println(e.getMessage)
    }
  }
  "safeClose" should "close once" in{

    expecting{ e => import e._
      exactly(2).of(connection).close()
    }
    
    val session = this.session()
    session.use()
    session.use()
    session.use()
    session.safeClose() must be(false)
    session.safeClose() must be(false)
    session.safeClose() must be(true)
    session.safeClose() must be(false)
    val session2 = this.session("2")
    session2.safeClose() must be(true)
    session2.safeClose() must be(false)
    mustThrowError(session2.use())

  }
  "forceClose" should "always close" in {

    expecting{ e => import e._
    exactly(2).of(connection).close()
    }

    val session = this.session()
    session.use()
    session.use()
    session.use()
    session.use()
    session.forceClose() must be(true)
    session.forceClose() must be(false)
    session.safeClose() must be(false)
    mustThrowError(session.use())
  }
  
  "transaction" should  "commit" in{

    expecting{ e => import e._
      allowing(connection).getAutoCommit();will(returnValue(true))
      allowing(connection).setAutoCommit(false)
      exactly(2).of(connection).commit
    }
    val session = this.session()
    session.commitTransaction() must be(false)
    session.beginTransaction()
    session.beginTransaction()
    session.beginTransaction()
    session.commitTransaction() must be(false)
    session.commitTransaction() must be(false)
    session.commitTransaction() must be(true)
    session.commitTransaction() must be(false)
    session.beginTransaction()
    session.beginTransaction()
    session.commitTransaction() must be(false)
    session.commitTransaction() must be(true)
  }

  "transaction" should "rollback" in{

    expecting{ e => import e._
    allowing(connection).getAutoCommit();will(returnValue(true))
    allowing(connection).setAutoCommit(false)
    exactly(2).of(connection).rollback
    }
    val session = this.session()
    session.rollback() must be(false)
    session.beginTransaction()
    session.beginTransaction()
    session.rollback() must be(true)
    session.rollback() must be(false)
    session.beginTransaction()
    session.beginTransaction()
    session.rollback() must be(true)
    session.rollback() must be(false)
  }

}