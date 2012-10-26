package org.squeryl.framework

import org.scalatest.matchers.ShouldMatchers
import org.squeryl.{SessionFactory, Session, Schema}

import org.squeryl.PrimitiveTypeMode._
import org.scalatest._
import org.scalatest.events.{TestIgnored, Ordinal}

abstract class SchemaTester extends DbTestBase{

  def schema : Schema

  def prePopulate() = {}
  
  override def withFixture(test: NoArgTest) {
    super.withFixture(new NoArgTest {
          def name = test.name
          def configMap = test.configMap
          def apply() { 
            _initSchema
           test.apply()
          }
    })
  }

  lazy val _initSchema = {
    if(SessionFactory.concreteFactory.isDefined){
      //println("Creating schema")
      transaction{
         schema.drop
         schema.create
        try{
          prePopulate
        }catch{
          case e : Exception =>
            println(e.getMessage)
            println(e.getStackTraceString)
        }
      }
    }
  }

  override def afterAll(){
    super.afterAll
    if(SessionFactory.concreteFactory.isDefined){
      transaction{
         schema.drop
      }
    }
  }
}

abstract class DbTestBase extends FunSuite with ShouldMatchers with BeforeAndAfterAll with BeforeAndAfterEach {

  def connectToDb : Option[() => Session]

  val ignoredTests : List[String] = Nil
  
  lazy val _initFactory = {
    //println("Initializing session factory")
    SessionFactory.concreteFactory = connectToDb
  }
  
  override def withFixture(test: NoArgTest) = {
    super.withFixture(new NoArgTest {
      override def name = test.name
      override def configMap = test.configMap
      override def apply() = {
	    _initFactory
	    if(SessionFactory.concreteFactory.isDefined && !ignoredTests.exists(_ == test.name)) {
	      //println("Running " + test.name + " with session factory " + SessionFactory.concreteFactory)
	      test.apply()
	    }
      }
    })
  }

}

