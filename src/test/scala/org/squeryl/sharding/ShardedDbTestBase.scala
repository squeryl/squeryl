package org.squeryl.sharding

import builder.SimpleShardedSessionBuilder
import org.scalatest._
import matchers.{MustMatchers, ShouldMatchers}
import org.squeryl.{PrimitiveTypeMode, SessionFactory}
import org.squeryl.framework.FileConfigReader

/**
 *
 * User: takeshita
 * Create: 12/01/22 22:32
 */

abstract class ShardedDbTestBase extends FunSuite with MustMatchers with BeforeAndAfterAll with BeforeAndAfterEach {

  def targetShards : List[String]

  def skipTest_? : Boolean

  def initializeSessions() : Boolean

  var notIgnored = true

  val ignoredTests : List[String] = Nil

  override def beforeAll(){
    super.beforeAll
    if(skipTest_?){
      println("Test:" + getClass() + " will be skipped")
      notIgnored = false
    }else{
      notIgnored = initializeSessions()
    }
  }

  override protected def runTest(testName: String,
                                 reporter: Reporter,
                                 stopper: Stopper,
                                 configMap: Map[String, Any],
                                 tracker: Tracker) {

    if(!notIgnored || ignoredTests.find(_ == testName).isDefined){
      //reporter(TestIgnored(new Ordinal(0), suiteName, Some(this.getClass.getName),testName))
      return
    }
    super.runTest(testName, reporter, stopper, configMap, tracker)
  }
}

trait SimpleShardingBuilderInitializer{

  lazy val config = {
    new FileConfigReader("org.squeryl.tests.cfg")
  }


  def skipTest_? : Boolean

  var _targetShards : List[String] = Nil
  def targetShards = _targetShards

  def initializeSessions() : Boolean = {

    println("Init debug shard sessions")
    val repos = new ShardedSessionRepositoryImpl()
    PrimitiveTypeMode.shardedSessionCache.shardedSessionRepository = repos
    for(settingSet <- shardSettings){
      println("Set shard:" + settingSet._1)
      val builder = createBuilder()
      
      builder.name = settingSet._1
      _targetShards = _targetShards :+ builder.name
      for(c <- settingSet._2){
        builder.addWriter(c)
      }
      for(c <- settingSet._3){
        builder.addReader(c)
      }
      repos.addFactory(builder.create())
    }

    true
  }

  def createBuilder() = {
    new SimpleShardedSessionBuilder()
  }

  /**
   *
   * List of
   *  (ShardName , WriterSettings , ReaderSettings
   *
   *
   */
  def shardSettings : List[(String, List[DatabaseConfig],List[DatabaseConfig])]

}
