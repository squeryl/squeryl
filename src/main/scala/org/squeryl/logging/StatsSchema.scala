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

import org.squeryl.KeyedEntity
import org.squeryl.Schema
import org.squeryl.dsl.CompositeKey2
import org.squeryl.PrimitiveTypeMode._

class StatementInvocation(
  val id: String,
  val start: Long,
  val end: Long,
  val statementHash: Int,
  val statementHashCollisionNumber: Int,
  val hostId: Int,
  val sessionId: Int,
  val rowCount: Option[Int],
  val iterationEndTime: Option[Long]) extends KeyedEntity[String] {

  def this(se: StatementInvocationEvent, _statementHash: Int, _hCollision: Int) =
    this(java.util.UUID.randomUUID.toString, se.start, se.end, _statementHash, _hCollision, 0, 0, None, None)

  def this() =
    this(null, 0, 0, 0, 0, 0, 0, Some(0), Some(0))

  def statementId =
    new CompositeKey2(statementHash, statementHashCollisionNumber)

  def executeTime =
    end minus start
}

object StatementHasher {

  private case class StatementCaseClass4HashGeneration(sql: String, definingClass: String, lineNumber: Int)

  def hash(sql: String, definingClass: String, lineNumber: Int): Int =
    StatementCaseClass4HashGeneration(sql, definingClass, lineNumber).hashCode
}

class Statement(val sql: String, val definingClass: String, val lineNumber: Int, val hash: Int, var statementHashCollisionNumber: Int) extends KeyedEntity[CompositeKey2[Int,Int]] {

  def this(sql: String, definingClass: String, lineNumber: Int) = {
    //TODO: support defining truncation rule in schena (on/declare)
    this(sql, definingClass, lineNumber, StatementHasher.hash(sql, definingClass, lineNumber), 0)
  }

  def this() = this("", "", 0, 0, 0)

  def id =
    new CompositeKey2(hash, statementHashCollisionNumber)
}

class StatLine(val statement: Statement, val avgExecTime: Double, val invocationCount: Long, val cumulativeExecutionTime: Long)

object Measure extends Enumeration {
   type Measure = Value
   val AvgExecTime, InvocationCount = Value
}

object StatsSchema extends Schema {

  override def drop = super.drop

  val statements = table[Statement]("Statementz")

  val statementInvocations = table[StatementInvocation]

  def invocationStats =
    from(statementInvocations)((si) =>
      groupBy(si.statementHash, si.statementHashCollisionNumber)
      compute(avg(si.executeTime), count, sum(si.executeTime))
    )

  import Measure._

  def topRankingStatements(topN: Int, measure: Measure) =
    from(invocationStats, statements)((si,s)=>
      where(si.key._1 === s.hash and si.key._2 === s.statementHashCollisionNumber)
      select(new StatLine(s, si.measures._1.get, si.measures._2, si.measures._3.get))
      orderBy(measure match {
        case AvgExecTime => si.measures._1.desc
        case InvocationCount => si.measures._2.desc
      })
    )
    .page(0, topN)

  on(statements)(s=> declare(
    s.sql is(dbType("clob")),
    s.definingClass is(dbType("varchar(512)"))
  ))  

  def recordStatementInvocationution(sie: StatementInvocationEvent) = {

    val statementK = _lookupOrCreateStatementAndReturnKey(sie)
    val si = new StatementInvocation(sie, statementK.a1, statementK.a2)

    statementInvocations.insert(si)
  }

  def recordResultSetIterationEnded(se: StatementInvocationEvent, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = {

    val s = new Statement(se.jdbcStatement, se.definingClass.getName, -1)

    val storedStatement = statements.lookup(s.id)
  }

  private def _lookupOrCreateStatementAndReturnKey(se: StatementInvocationEvent) = {

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