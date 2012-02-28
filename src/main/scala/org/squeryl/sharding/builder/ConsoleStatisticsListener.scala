package org.squeryl.sharding.builder

import org.squeryl.logging.{StatementInvocationEvent, StatisticsListener}

/**
 * Print sql to console
 * User: takeshita
 * Date: 11/09/09
 * Time: 2:40
 * To change this template use File | Settings | File Templates.
 */

object ConsoleStatisticsListener extends StatisticsListener{

  def toElapsed(se : StatementInvocationEvent) = {
    (se.end - se.start) + " msec"
  }

  def queryExecuted(se: StatementInvocationEvent) = {
    println(toElapsed(se) + " : " + se.jdbcStatement)
  }

  def resultSetIterationEnded(statementInvocationId: String, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) {}

  def updateExecuted(se: StatementInvocationEvent) = {
    println(toElapsed(se) + " : " + se.jdbcStatement)
  }
  def insertExecuted(se: StatementInvocationEvent) = {
    println(toElapsed(se) + " : " + se.jdbcStatement)
  }
  def deleteExecuted(se: StatementInvocationEvent) = {
    println(toElapsed(se) + " : " + se.jdbcStatement)
  }
}