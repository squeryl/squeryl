/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.tests

import _root_.org.squeryl.Session
import org.squeryl.Query


trait QueryTester {

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

  def log(queryName: Symbol, query:Query[_]) = {

    println(queryName + " :")
    println(query)

    for(r <- query)
      println("-->" + r)
  }

  def assertEquals[E](expected:E, actual:E, s:Symbol): Unit =
    assertEquals(expected, actual, s.toString)

  def assertEquals[E](expected:E, actual:E, msg:String): Unit =
    if(expected != actual)
      error("expected " + expected + " got " + actual)

  def validateQuery[R,S](name: Symbol, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit =
    validateQuery[R,S](logQueries, name, q, mapFunc, expected)

  def validateQuery[R,S](logFirst: Boolean, name: Symbol, q:Query[R], mapFunc: R=>S, expected: List[S]): Unit = {

    if(validateFirstAndExit >= 1)
      return

//    if(dumpAst)
//      println(q.dumpAst)
    
    if(logFirst || logQueries)
      log(name, q)

    if(doNotExecute)
      return

    val r = q.toList.map(mapFunc)

    if(r == expected)
      println("query " + name + " passed.")
    else {
      val msg =
        "query : " + name + " failed,\n" +
        "expected " + expected + " got " + r + " \n query " + name +
        " was : \n" + q
      error(msg)
    }
    
    if(validateFirstAndExit >= 0)
      validateFirstAndExit += 1
  }

  def passed(s: Symbol) = println(s + " passed.")
}
