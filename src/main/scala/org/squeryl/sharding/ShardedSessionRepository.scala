package org.squeryl.sharding

import org.squeryl.internals.DatabaseAdapter
import com.mysql.jdbc.Connection
import org.squeryl.logging.StatisticsListener
import java.lang.ThreadLocal
import org.squeryl.{SquerylException, SessionFactory, Session}

/**
 * Created by IntelliJ IDEA.
 * User: takezou
 * Date: 11/09/04
 * Time: 19:54
 * To change this template use File | Settings | File Templates.
 */

class DatabaseConfig(
  var url : String,                       
  var username : Option[String] = None,
  var password : Option[String] = None  ){

}
trait ShardedSessionFactory{

  val shardName : String
  def databaseAdapter : DatabaseAdapter
  def connectionManager : ConnectionManager

  var statisticsListener : Option[() => StatisticsListener] = None

  def reader(index : Int) : ShardedSession = session(ShardMode.Read,index)
  def writer(index : Int) : ShardedSession = session(ShardMode.Write,index)

  def session(mode : ShardMode.Value , index : Int) : ShardedSession = {
    
    def toShardedSession(session  :Session) = {
      ShardedSessionImpl(shardName,mode,session)
    }
    
    val config = this.config(mode,index)
    if(config == null){
      throw new DatabaseConfigNotFoundException(shardName,mode.toString,index)
    }
    val session = if(statisticsListener.isDefined){
      new Session(connectionManager.connection(shardName,mode,config),databaseAdapter,
        Some(statisticsListener.get()))
    }else{
      Session.create( connectionManager.connection(shardName,mode,config),databaseAdapter)
    }
    toShardedSession(session)
  }

  def selectReader : ShardedSession
  def selectWriter : ShardedSession

  def getReaderConfig(index : Int ) : DatabaseConfig = config(ShardMode.Read,index)

  def getWriterConfig(index : Int) : DatabaseConfig = config(ShardMode.Write,index)

  def config(mode : ShardMode.Value , index : Int) : DatabaseConfig



}

class DatabaseConfigNotFoundException(shardName : String , modeName  : String , index : Int) extends
Exception("Databse config for shard:%s mode:%s index:%s".format(shardName,modeName,index))


trait ShardedSessionRepository{

  def allShardNames : List[String]
  def allFactories : List[ShardedSessionFactory]

  def apply(name : String , mode : ShardMode.Value) : ShardedSession
  def addFactory(factory : ShardedSessionFactory) : Unit

}

class ShardedSessionRepositoryImpl extends ShardedSessionRepository {

  private var shardedSessionFactories = Map[String,ShardedSessionFactory]()

  def allFactories = shardedSessionFactories.values.toList
  def allShardNames = shardedSessionFactories.keys.toList

  def addFactory(shardingSession : ShardedSessionFactory)  = {
    if(SessionFactory.concreteFactory.isEmpty){
      // register first ShardedSessionFactory as default squeryl session.
      SessionFactory.concreteFactory = shardingSession.selectWriter match{
        case ShardedSessionImpl(name,mode,session) => Some(() => session)
        case _ => None
      }
    }
    shardedSessionFactories +=( shardingSession.shardName -> shardingSession)
  }

  def apply(shardName : String, mode : ShardMode.Value) : ShardedSession = {
    try{
      shardedSessionFactories(shardName).session(mode,0)
    }catch{
      case e : NoSuchElementException => {
        throw new ShardNotFoundException(shardName)
      }
    }
  }


}