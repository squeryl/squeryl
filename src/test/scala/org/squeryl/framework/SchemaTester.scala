package org.squeryl.framework

import org.squeryl.{SessionFactory, Schema}

import org.squeryl.test.PrimitiveTypeModeForTests._
import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

abstract class SchemaTester extends DbTestBase {
  self: DBConnector =>

  def schema: Schema

  def prePopulate() = {}

  override def beforeAll() = {

    super.beforeAll()

    sessionCreator().foreach { _ =>
      transaction {
        schema.drop
        schema.create
        try {
          prePopulate()
        } catch {
          case e: Exception =>
            e.printStackTrace()
        }
      }
    }
  }

  override def afterAll() = {
    super.afterAll()

    sessionCreator().foreach { _ =>
      transaction {
        schema.drop
      }
    }
  }
}

abstract class DbTestBase extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach with Matchers {
  self: DBConnector =>

  def isIgnored(testName: String) =
    sessionCreator().isEmpty || ignoredTests.exists(_ == testName)

  def ignoredTests: List[String] = Nil

  override def beforeAll() = {
    val c = sessionCreator()
    if (c.isDefined) {
      SessionFactory.concreteFactory = c
    }
  }

  override protected def runTest(testName: String, args: org.scalatest.Args): org.scalatest.Status = {
    if (isIgnored(testName))
      org.scalatest.SucceededStatus
    else
      super.runTest(testName, args)
  }

}
