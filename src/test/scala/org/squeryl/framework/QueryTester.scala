package org.squeryl.framework

import org.squeryl.{Session, Query}
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.scalatest.Matchers

trait QueryTester extends Matchers {

  var logQueries = false

  var validateFirstAndExit = -1

  var dumpAst = false

  var doNotExecute = false

  def activateWorkbenchMode(): Unit = {
    logQueries = true
    dumpAst = true
    validateFirstAndExit = 0
  }

  def loggerOn(): Unit =
    Session.currentSession.setLogger((s:String) => println(s))

  def log(queryName: Symbol, query:Query[_]): Unit = {

    println(queryName + " :")
    println(query)

    for(r <- query.asInstanceOf[Query[Any]])
      println("-->" + r)
  }

  def assertEquals[E](expected:E, actual:E, s:Symbol): Unit =
    assertEquals(expected, actual, s.toString)

  def assertEquals[E](expected:E, actual:E, msg:String): Unit = withClue(msg) {
    actual shouldBe expected
  }

  def validateQuery[R,S](name: Symbol, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit =
    validateQuery[R,S](logQueries, name, q, mapFunc, expected)

  def validateQuery[R,S](logFirst: Boolean, name: Symbol, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit = {

    if(validateFirstAndExit >= 1)
      return

    if(logFirst || logQueries)
      log(name, q)

    if(doNotExecute)
      return

    val r = q.toList.map(mapFunc)

    r should equal(expected)

    if(validateFirstAndExit >= 0)
      validateFirstAndExit += 1
  }

  def passed(s: Symbol): Unit = println(s)
}

object SingleTestRun extends org.scalatest.Tag("SingleTestRun")
