package org.squeryl.framework

import org.squeryl.{Session, Query}
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.scalatest.matchers.should.Matchers

trait QueryTester extends Matchers {

  var logQueries = false

  var validateFirstAndExit = -1

  var dumpAst = false

  var doNotExecute = false

  def activateWorkbenchMode = {
    logQueries = true
    dumpAst = true
    validateFirstAndExit = 0
  }

  def loggerOn =
    Session.currentSession.setLogger((s:String) => println(s))

  def log(queryName: String, query:Query[_]) = {

    println(queryName + " :")
    println(query)

    for(r <- query.asInstanceOf[Query[Any]])
      println("-->" + r)
  }

  def validateQuery[R,S](name: String, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit =
    validateQuery[R,S](logQueries, name, q, mapFunc, expected)

  def validateQuery[R,S](logFirst: Boolean, name: String, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit = {

    if(validateFirstAndExit >= 1)
      return

//    if(dumpAst)
//      println(q.dumpAst)

    if(logFirst || logQueries)
      log(name, q)

    if(doNotExecute)
      return

    val r = q.toList.map(mapFunc)

    r should equal(expected)

//    if(r == expected)
//      println("query " + name + " passed.")
//    else {
//      val msg =
//        "query : " + name + " failed,\n" +
//        "expected " + expected + " got " + r + " \n query " + name +
//        " was : \n" + q
//      org.squeryl.internals.Utils.org.squeryl.internals.Utils.throwError(msg)
//    }

    if(validateFirstAndExit >= 0)
      validateFirstAndExit += 1
  }
}

object SingleTestRun extends org.scalatest.Tag("SingleTestRun")
