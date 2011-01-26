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


class StatementInvocationEvent(_definitionOrCallSite: StackTraceElement, val start: Long, val end: Long, val rowCount: Int, val jdbcStatement: String) {

  val uuid = {
    val tmp = java.util.UUID.randomUUID
    java.lang.Long.toHexString(tmp.getMostSignificantBits) + "-" +
    java.lang.Long.toHexString(tmp.getLeastSignificantBits)
  }

  def definitionOrCallSite =
    _definitionOrCallSite.toString
}

trait StatisticsListener {

  def queryExecuted(se: StatementInvocationEvent): Unit

  def resultSetIterationEnded(statementInvocationId: String, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean): Unit

  def updateExecuted(se: StatementInvocationEvent): Unit

  def insertExecuted(se: StatementInvocationEvent): Unit

  def deleteExecuted(se: StatementInvocationEvent): Unit
}


object StackMarker {

  def lastSquerylStackFrame[A](a: =>A) = a
}


