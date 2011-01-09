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


class StatementExecution(_definingClass: Class[_], val start: Long, val end: Long, val rowCount: Int, val jdbcStatement: String) {


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

  def queryExecuted(se: StatementExecution): Unit

  def resultSetIterationEnded(se: StatementExecution, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean): Unit

  def updateExecuted(se: StatementExecution): Unit

  def insertExecuted(se: StatementExecution): Unit

  def deleteExecuted(se: StatementExecution): Unit
}

object StackMarker {

  def lastSquerylStackFrame[A](a: =>A) = a
}


class IterationEnd(val iterationCompleted: Boolean, val iterationEndTime: Long)

class StatementExec(
 val start: Long, val end: Long, statementHash: Int, statementHashCollisionNumber: Int, val numRows: Int,
 val hostId: Int, val sessionId: Int, val guidHelper: Int) extends KeyedEntity[CompositeKey2[Long, Int]] {

  def this(se: StatementExecution, _statementHash: Int, _hCollision: Int) =
    this(se.start, se.end, _statementHash, _hCollision, se.rowCount, 0, 0, 0)

  def id =
    new CompositeKey2(start, guidHelper)

  def statementId =
    new CompositeKey2(statementHash, statementHashCollisionNumber)
}

case class StatementCase(val sql: String, val definingClass: String, val lineNumber: Int)

case class Statement(val sql: String, val definingClass: String, val lineNumber: Int, val hash: Int, var statementHashCollisionNumber: Int) extends KeyedEntity[CompositeKey2[Int,Int]] {

  def this(sql: String, definingClass: String, lineNumber: Int) =
    this(sql, definingClass, lineNumber, StatementCase(sql, definingClass, lineNumber).hashCode, 0)

  def this() = this("", "", 0, 0, 0)

  def id =
    new CompositeKey2(hash, statementHashCollisionNumber)
}

object StatsSchema extends Schema {

  import org.squeryl.PrimitiveTypeMode._

  val statements = table[Statement]

  val statementExecs = table[StatementExec]

  def recordStatementExecution(se: StatementExecution) = {

    val statementK = lookupStatementKey(se)
    val s0 = new StatementExec(se, statementK.a1, statementK.a2)

    statementExecs.insert(s0)
  }

  def lookupStatementKey(se: StatementExecution) = {

    val s = new Statement(se.jdbcStatement, se.definingClass.getName, -1)

    val storedStatement = statements.lookup(s.id)

    val s2 =
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

    s2.id
  }
}