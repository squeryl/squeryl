package org.squeryl.logging

import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._

object LocalH2SinkStatisticsListener {

  def initializeOverwrite(schemaFile: String)= {
    Class.forName("org.h2.Driver");

    val file = new java.io.File(schemaFile)

    if(file.exists)
      file.delete

    val s = new Session(
      java.sql.DriverManager.getConnection("jdbc:h2:" + schemaFile, "sa", ""),
      new H2Adapter)

    using(s) {
      StatsSchema.drop
      StatsSchema.create
    }
      
    val l = new LocalH2SinkStatisticsListener(s)
    l
  }
}

class LocalH2SinkStatisticsListener(val h2Session: Session) extends StatisticsListener {


  private val _queue = new java.util.concurrent.ArrayBlockingQueue[()=>Unit](1024, false)

  private val _worker = new Thread {

    override def run() {
      h2Session.bindToCurrentThread
      while(true) {
        val op = _queue.take
        op()
      }
    }
  }

  _worker.start

  private def _pushOp(op: =>Unit) =
    _queue.put(op _)

  def generateStatSummary(staticHtmlFile: java.io.File, n: Int) = _pushOp {
    BarChartRenderer.generateStatSummary(staticHtmlFile, n)
  }

  def queryExecuted(se: StatementInvocationEvent) =_pushOp {
    StatsSchema.recordStatementInvocation(se)
    h2Session.connection.commit
  }

  def resultSetIterationEnded(invocationId: String, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = _pushOp {
    StatsSchema.recordEndOfIteration(invocationId, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean)
    h2Session.connection.commit
  }

//  def generateStatSummary(staticHtmlFile: java.io.File, n: Int) = using(h2Session) {
//    BarChartRenderer.generateStatSummary(staticHtmlFile, n)
//  }
//
//  def queryExecuted(se: StatementInvocationEvent) = using(h2Session) {
//    StatsSchema.recordStatementInvocation(se)
//    h2Session.connection.commit
//  }
//
//  def resultSetIterationEnded(invocationId: String, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = using(h2Session) {
//    StatsSchema.recordEndOfIteration(invocationId, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean)
//    h2Session.connection.commit
//  }

  def updateExecuted(se: StatementInvocationEvent) = {}

  def insertExecuted(se: StatementInvocationEvent) = {}

  def deleteExecuted(se: StatementInvocationEvent) = {}
}