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

  def generateStatSummary(staticHtmlFile: java.io.File, n: Int) = using(h2Session) {
    BarChartRenderer.generateStatSummary(staticHtmlFile, n)
  }

  def queryExecuted(se: StatementInvocationEvent) = using(h2Session) {
    StatsSchema.recordStatementInvocationution(se)
    h2Session.connection.commit
  }

  def resultSetIterationEnded(se: StatementInvocationEvent, iterationEndTime: Long, rowCount: Int, iterationCompleted: Boolean) = {}

  def updateExecuted(se: StatementInvocationEvent) = {}

  def insertExecuted(se: StatementInvocationEvent) = {}

  def deleteExecuted(se: StatementInvocationEvent) = {}
}