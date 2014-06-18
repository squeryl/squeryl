package org.squeryl.framework

import org.squeryl.{SessionFactory, Session, Schema}
import org.squeryl.PrimitiveTypeMode._
import org.scalatest._
import org.scalatest.events.{TestIgnored, Ordinal}
import org.scalatest.matchers.ShouldMatchers

abstract class SchemaTester extends DbTestBase {
  self: DBConnector =>

  def schema : Schema

  def prePopulate() = {}

  override def beforeAll(){

    super.beforeAll()

    sessionCreator().foreach { _ =>
      transaction {
        schema.drop
        schema.create
        try{
          prePopulate
        }catch{
          case e : Exception =>
            println(e.getMessage)
            println(e.getStackTrace)
        }
      }
    }
  }

  override def afterAll(){
    super.afterAll()

    sessionCreator().foreach { _ =>
      transaction {
        schema.drop
      }
    }
  }
}

abstract class DbTestBase extends FunSuite with BeforeAndAfterAll with BeforeAndAfterEach with ShouldMatchers {
  self: DBConnector =>

  def isIgnored(testName: String) =
    sessionCreator().isEmpty || ignoredTests.exists(_ == testName)

  def ignoredTests : List[String] = Nil

  override def beforeAll(){
    val c = sessionCreator()
    if(c.isDefined) {
      SessionFactory.concreteFactory = c
    }
  }

  override protected def runTest(testName: String,args: org.scalatest.Args): org.scalatest.Status = {
    if(isIgnored(testName))
      org.scalatest.SucceededStatus
    else
      super.runTest(testName, args)
  }

}

