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

object StatsSchemaTypeMode extends org.squeryl.PrimitiveTypeMode
import StatsSchemaTypeMode._

/**
 * id is a UUID generatted by java.util.UUID
 */
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
    this(se.uuid, se.start, se.end, _statementHash, _hCollision, 0, 0, None, None)

  def this() =
    this(null, 0, 0, 0, 0, 0, 0, Some(0), Some(0))

  def statementId =
    new CompositeKey2(statementHash, statementHashCollisionNumber)

  def executeTime =
    end minus start
}

object StatementHasher {

  private case class StatementCaseClass4HashGeneration(sql: String, definitionOrCallSite: String)

  def hash(sql: String, definitionOrCallSite: String): Int =
    StatementCaseClass4HashGeneration(sql, definitionOrCallSite).hashCode
}

class Statement(val sql: String, val definitionOrCallSite: String, val hash: Int, var statementHashCollisionNumber: Int) extends KeyedEntity[CompositeKey2[Int,Int]] {

  def this(sql: String, definitionOrCallSite: String) = {
    //TODO: support defining truncation rule in schena (on/declare)
    this(sql, definitionOrCallSite, StatementHasher.hash(sql, definitionOrCallSite), 0)
  }

  def this() = this("", "", 0, 0)

  def id =
    new CompositeKey2(hash, statementHashCollisionNumber)
}

class StatLine(val statement: Statement, val avgExecTime: Double, val invocationCount: Long, val cumulativeExecutionTime: Long, val avgRowCount: Float) {
  def definitionSite =
    statement.definitionOrCallSite
}

object Measure extends Enumeration {
   type Measure = Value
   val AvgExecTime, InvocationCount, CumulativeExecutionTime, AvgResultSetSize = Value
} 

object StatsSchema extends Schema {

  override def drop = super.drop

  val statements = table[Statement]("Statementz")

  val statementInvocations = table[StatementInvocation]

  def invocationStats =
    from(statementInvocations)((si) =>
      groupBy(si.statementHash, si.statementHashCollisionNumber)
      compute(avg(si.executeTime), count, sum(si.executeTime), nvl(avg(si.rowCount),0))
    )

  import Measure._

  def topRankingStatements(topN: Int, measure: Measure) =
    from(invocationStats, statements)((si,s)=>
      where(si.key._1 === s.hash and si.key._2 === s.statementHashCollisionNumber)
      select(new StatLine(s, si.measures._1.get, si.measures._2, si.measures._3.get, si.measures._4))
      orderBy(measure match {
        case AvgExecTime => si.measures._1.desc
        case InvocationCount => si.measures._2.desc
        case CumulativeExecutionTime => si.measures._3.desc
        case AvgResultSetSize => si.measures._4.desc
      })
    )
    .page(0, topN)

  on(statements)(s=> declare(
    s.sql is(dbType("clob")),
    s.definitionOrCallSite is(dbType("varchar(512)"))
  ))  

  def recordStatementInvocation(sie: StatementInvocationEvent) = {

    val statementK = _lookupOrCreateStatementAndReturnKey(sie)
    val si = new StatementInvocation(sie, statementK.a1, statementK.a2)
    statementInvocations.insert(si)
    si.id
  }

  def recordEndOfIteration(statementInvocationId: String, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = {

    update(statementInvocations)(si =>
      where(si.id === statementInvocationId)
      set(si.iterationEndTime := Some(iterationEndTime), si.rowCount := Some(rowCount))
    )
  }

  private def _lookupOrCreateStatementAndReturnKey(se: StatementInvocationEvent) = {

    val s = new Statement(se.jdbcStatement, se.definitionOrCallSite)

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