package org.squeryl.tests


import _root_.org.specs.Specification
import _root_.org.squeryl.adapters.OracleAdapter
import _root_.org.squeryl.{Schema, KeyedEntity, Session, SessionFactory}
import org.specs._
import scala.collection.mutable.HashSet


class ComponentTests extends Specification {

  val oa = new OracleAdapter

  classOf[OracleAdapter].getName should {

    val possibilities = oa.paddingPossibilities("xxx", 3)

    "have method 'paddingPossibilities' generate unique sequences " in {
      val set = possibilities.toSet
      possibilities must haveSize(set.size)
    }

    "have method 'paddingPossibilities' generate strings with prescribed length " in {
      possibilities.filter(_.length != 6) must beEqualTo(Nil)
    }

    "make String identifier unique in a given Set, by padding right side" in {

      val scope = new HashSet[String]

      val setOfPossibleUniqeIdsWithAB = oa.legalOracleSuffixChars.size * 2 + 1

      for(i <- 1 to setOfPossibleUniqeIdsWithAB) {
        val id = oa.makeUniqueInScope("AB", scope)
        scope.add(id)
      }

      scope must haveSize(setOfPossibleUniqeIdsWithAB)
    }

    "shrink identifiers and preserve uniqueness in a given Set" in {
      shrinkTestSet must haveSize(50)
    }

    "shrink identifiers to a length of 30" in {
      shrinkTestSet.filter(_.length != 30) must haveSize(0)
    }
  }  

  def shrinkTestSet = {
    val scope = new HashSet[String]
    for(i <- 1 to 50)
      oa.shrinkTo30AndPreserveUniquenessInScope("UYTFBTFBTFNIUNGONGONYGNOYNGOYGN&%DTFKTFIO&%DTFKTFIO", scope)
    scope
  }
}

class BasicORMTestsOnH2 extends Specification {

  "the Basic ORM tests " should {

    "not throw an exception " in {
      org.squeryl.tests.Tests.allTestsOnH2
      //"a" mustMatch "a"
    }
  }
}





class Foo(val value: String) extends KeyedEntity[Long] {
  val id: Long = 0
}

object FooSchema extends Schema {
  val foos = table[Foo]

  def reset() = {
    drop // its protected for some reason
    create
  }
}

object TransactionsTests {
  import org.squeryl.PrimitiveTypeMode._

  def allTests(s: ()=> Session) = {

    SessionFactory.concreteFactory = Some(s)
    test1
    test2
    test3
  }

  def throwExc(except: Boolean): Int = {
    if(except) throw new Exception()
    return 1
  }

  def doSomething(except: Boolean) : Int = {
    transaction{
      throwExc(except)
    }
  }

  def test1 = {
    transaction {
      FooSchema.reset
      val foo1 = FooSchema.foos.insert(new Foo("test"))
      assert(FooSchema.foos.where(f => f.value === "test").size == 1 )

      try {
        doSomething(true)
      }
      catch {
        case e: Exception => {}
      }

      // fails with "no session exception"
      assert(FooSchema.foos.where(f => f.value === "test").size ==1)
    }

    println("Excepting out of nested transaction passed.")
  }

  def test2 =  {
    transaction {
        FooSchema.reset
        val foo1 = FooSchema.foos.insert(new Foo("test"))
        assert(FooSchema.foos.where(f => f.value === "test").size ==1)//should equal(1)

        doSomething(false)
        // fails with "no session exception"
        FooSchema.foos.where(f => f.value === "test").size //should equal(1)
    }
    println("Returning out of nested transaction passed.")
  }

  def test3 = {
    transaction{
      FooSchema.reset
      val foo1 = FooSchema.foos.insert(new Foo("test"))
      assert(FooSchema.foos.where(f => f.value === "test").size == 1)//should equal(1)

      doSomething(false)
    }
    transaction{
      // works!
      assert(FooSchema.foos.where(f => f.value === "test").size == 1)//should equal(1)
    }

    println("Returning out of sibling transaction passed.")
  }
}