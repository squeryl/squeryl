package org.squeryl.sharding.scenario

import java.util.Date
import org.squeryl.sharding.ShardedSchemaTester
import org.scalatest.matchers.MustMatchers
import org.squeryl.{PrimitiveTypeMode, Schema, KeyedEntity}

/**
 *
 * User: takeshita
 * Create: 12/01/22 23:07
 */

class User(var id : Long,  var name : String) extends KeyedEntity[Long]{
  def this() = this(0,"")
}

class Blog(var id : Long , var writerId : Long,text : String,  date : Date) extends KeyedEntity[Long]{
  def this() = this(0,0,"",new Date)
}

object BlogDb extends Schema{

  import PrimitiveTypeMode._

  val users = table[User]
  val blogs = table[Blog]

  // turn off auto increment
  on(users)(t => {
    declare(t.id is(primaryKey))
  })
  on(blogs)(t => {
    declare(t.id is(primaryKey))
  })
  
}


abstract class BlogDbTestRun extends ShardedSchemaTester {
  import org.squeryl.PrimitiveTypeMode._
  import BlogDb._

  def schema: Schema = BlogDb


  def selectShard(userId : Long) = {
    targetShards( (userId % targetShards.size).toInt)
  }

  //"users" should "created" in {
  test("save to selected shard") {

    def createUser(userId : Long) = {
      write(selectShard(userId)){
        users.insert(new User(userId,"user" + userId))
      }
    }
    val userIds = List(1L,2L,5L,6L,8L,2325L,132L)
    userIds.foreach(createUser(_))

    for(userId <- userIds){
      val myShard = selectShard(userId)
      for( shard <- targetShards){
        read(shard){
          if(shard == myShard){
            users.lookup(userId) must not be(None)
          }else{
            users.lookup(userId) must be(None)
          }
        }
      }
    }


  }



}