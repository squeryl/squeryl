package org.squeryl.logging

import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import org.squeryl.InternalFieldMapper._

object LocalH2SinkStatisticsListener {

  def initializeOverwrite(schemaName: String, workingDir: String = ".") =
    initialize(schemaName, true, workingDir)

  def initializeAppend(schemaName: String, workingDir: String = ".") =
    initialize(schemaName, false, workingDir)

  def initialize(schemaName: String, overwrite: Boolean, workingDir: String) = {
    Class.forName("org.h2.Driver");

    val file = new java.io.File(workingDir, schemaName + ".h2.db").getCanonicalFile
    val exists = file.exists

    if(file.exists && overwrite)
      file.delete

    val s = new Session(
      java.sql.DriverManager.getConnection("jdbc:h2:" + workingDir + "/" + schemaName, "sa", ""),
      new H2Adapter)

    if((!file.exists) || overwrite)
      using(s) {
        StatsSchema.create
      }
      
    val l = new LocalH2SinkStatisticsListener(s)
    l
  }
}

class LocalH2SinkStatisticsListener(val h2Session: Session) extends StatisticsListener {

  private var _closed = false

  private val _queue = new java.util.concurrent.ArrayBlockingQueue[()=>Unit](1024, false)

  private val _worker = new Thread {

    override def run() {
      h2Session.bindToCurrentThread
      while(!_closed) {
        val op = _queue.take
        op()
      }
    }
  }

  _worker.start

  def shutdown = _closed = true

  private def _pushOp(op: =>Unit) =
    if(!_closed) {
      _queue.put(op _)
    }
    else
      throw new IllegalStateException('LocalH2SinkStatisticsListener + " has been shutdown.")

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

  def updateExecuted(se: StatementInvocationEvent) = {}

  def insertExecuted(se: StatementInvocationEvent) = {}

  def deleteExecuted(se: StatementInvocationEvent) = {}
}