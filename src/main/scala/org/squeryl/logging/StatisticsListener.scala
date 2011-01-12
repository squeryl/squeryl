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
 ******************************************************************************/
package org.squeryl.logging

import org.squeryl.dsl.ast.ExpressionNode
import org.squeryl.internals.Utils
import org.squeryl.dsl.CompositeKey2
import org.squeryl.{Schema, KeyedEntity}


class StatementInvocationEvent(_definingClass: Class[_], val start: Long, val end: Long, val rowCount: Int, val jdbcStatement: String) {


  /**
   * The use of this method is to allow to unambiguously link the statement execution the code
   * that calls this statement.
   *
   * select, compute, groupBy statements always have closure, this is the class returned by this
   * method for this kind of statements. For statements like Table[A] (.lookup, .delete, .update(a:A))
   * the defining class is the A class within Table[A] (or View[A]).
   */
  def definingClass: Class[_] = _definingClass

 /**
  * The use of this method is to allow to unambiguously link the statement execution the code
  * that calls this statement.
  */
  
  def callSite: StackTraceElement = {

    val st = Thread.currentThread.getStackTrace
    var i = 0
    while(st.length < i) {
      val e = st(i)
      // TODO : make top level call in a method who's only purpose is to identify the call site in the user code ?
      if(e.getClassName.startsWith("org.squeryl.") || e.getClassName.startsWith("scala."))
        i = i + 1
      else
        return e
    }

    error("could not find stack element")
  }
}  

trait StatisticsListener {

  def queryExecuted(se: StatementInvocationEvent): Unit

  def resultSetIterationEnded(se: StatementInvocationEvent, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean): Unit

  def updateExecuted(se: StatementInvocationEvent): Unit

  def insertExecuted(se: StatementInvocationEvent): Unit

  def deleteExecuted(se: StatementInvocationEvent): Unit
}


class LocalH2SinkStatisticsListener extends StatisticsListener {

  def queryExecuted(se: StatementInvocationEvent) = {
    
  }

  def resultSetIterationEnded(se: StatementInvocationEvent, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = {

  }

  def updateExecuted(se: StatementInvocationEvent) = {}

  def insertExecuted(se: StatementInvocationEvent) = {}

  def deleteExecuted(se: StatementInvocationEvent) = {}
}


object StackMarker {

  def lastSquerylStackFrame[A](a: =>A) = a
}


class IterationEnd(val iterationCompleted: Boolean, val iterationEndTime: Long)

class StatementInvocation(
 val start: Long, val end: Long, statementHash: Int, statementHashCollisionNumber: Int, val numRows: Int,
 val hostId: Int, val sessionId: Int, val guidHelper: Int) extends KeyedEntity[CompositeKey2[Long, Int]] {

  def this(se: StatementInvocationEvent, _statementHash: Int, _hCollision: Int) =
    this(se.start, se.end, _statementHash, _hCollision, se.rowCount, 0, 0, 0)

  def id =
    new CompositeKey2(start, guidHelper)

  def statementId =
    new CompositeKey2(statementHash, statementHashCollisionNumber)
}

object StatementHasher {

  private case class StatementCaseClass4HashGeneration(val sql: String, val definingClass: String, val lineNumber: Int)

  def hash(sql: String, definingClass: String, lineNumber: Int): Int =
    StatementCaseClass4HashGeneration(sql, definingClass, lineNumber).hashCode
}

class Statement(val sql: String, val definingClass: String, val lineNumber: Int, val hash: Int, var statementHashCollisionNumber: Int) extends KeyedEntity[CompositeKey2[Int,Int]] {

  def this(sql: String, definingClass: String, lineNumber: Int) =
    this(sql, definingClass, lineNumber, StatementHasher.hash(sql, definingClass, lineNumber), 0)

  def this() = this("", "", 0, 0, 0)

  def id =
    new CompositeKey2(hash, statementHashCollisionNumber)
}

object StatsSchema extends Schema {

  import org.squeryl.PrimitiveTypeMode._

  val statements = table[Statement]

  val statementInvocations = table[StatementInvocation]

  def recordStatementInvocationution(sie: StatementInvocationEvent) = {

    val statementK = _lookupOrCreateStatementInvocationAndReturnKey(sie)
    val si = new StatementInvocation(sie, statementK.a1, statementK.a2)

    statementInvocations.insert(si)
  }

  private def _lookupOrCreateStatementInvocationAndReturnKey(se: StatementInvocationEvent) = {

    val s = new Statement(se.jdbcStatement, se.definingClass.getName, -1)

    val storedStatement = statements.lookup(s.id)

    val result =
      if(storedStatement == None) {
        statements.insert(s)
        s
      }
      else {
        val q =
          from(statements)(st =>
            where(st.hash === s.hash)
            select(st)
            orderBy(st.statementHashCollisionNumber)
          )

        var lastCollisionNum = -1
        val mathingStatement =
          q.find(st => {
            lastCollisionNum = st.statementHashCollisionNumber
            st == s
          })

        if(mathingStatement != None)
          mathingStatement.get
        else {
          s.statementHashCollisionNumber = lastCollisionNum + 1
          statements.insert(s)
          s
        }
      }

    result.id
  }
}