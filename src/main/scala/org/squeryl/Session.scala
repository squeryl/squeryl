package org.squeryl

import org.squeryl.internals._
import collection.mutable.ArrayBuffer
import java.sql.{SQLException, ResultSet, Statement, Connection}


trait Session {
  
  def connection: Connection

  def databaseAdapter: DatabaseAdapter

  def bindToCurrentThread = Session.currentSession = Some(this)

  def unbindFromCurrentThread = Session.currentSession = None

  private var _logger: String => Unit = null

  def logger_=(f: String => Unit) = _logger = f

  def setLogger(f: String => Unit) = _logger = f

  def isLoggingEnabled = _logger != null

  def log(s:String) = if(isLoggingEnabled) _logger(s)

  private val _statements = new ArrayBuffer[Statement]

  private val _resultSets = new ArrayBuffer[ResultSet]

  private [squeryl] def _addStatement(s: Statement) = _statements.append(s)

  private [squeryl] def _addResultSet(rs: ResultSet) = _resultSets.append(rs)

  private def _closeStatement(s: Statement) =
    try {s.close}
    catch {case e:SQLException => {}}

  private [squeryl] def _closeResultSet(rs: ResultSet) =
    try {rs.close}
    catch {case e:SQLException => {}}

  def cleanup = {
    _statements.foreach(s => _closeStatement(s))
    _statements.clear
    _resultSets.foreach(rs => _closeResultSet(rs))
    _resultSets.clear
  }
}

trait SessionFactory {
  def newSession: Session
}

object SessionFactory {

  /**
   * Initializing concreteFactory with a Session creating closure enables the use of
   * the 'transaction' and 'inTransaction' block functions 
   */
  var concreteFactory: Option[()=>Session] = None

  /**
   * Initializing externalTransactionManagementAdapter with a Session creating closure allows to
   * execute Squeryl statements *without* the need of using 'transaction' and 'inTransaction'.
   * The use case for this is to allow Squeryl connection/transactions to be managed by an
   * external framework. In this case Session.cleanupResources *needs* to be called when connections
   * are closed, otherwise statement of resultset leaks can occur. 
   */
  var externalTransactionManagementAdapter: Option[()=>Session] = None

  def newSession: Session =
      concreteFactory.getOrElse(
        error("org.squeryl.SessionFactory not initialized, SessionFactory.concreteFactory must be assigned a \n"+
              "function for creating new org.squeryl.Session, before transaction can be used.\n" +
              "Alternatively SessionFactory.externalTransactionManagementAdapter can initialized, please refer to the documentation.")
      ).apply        
}

object Session {

  private val _currentSessionThreadLocal = new ThreadLocal[Option[Session]] {
    override def initialValue = None
  }
  
  def create(c: Connection, a: DatabaseAdapter) = new Session {
    def connection = c
    def databaseAdapter = a
  }

  def currentSession: Session =
    if(SessionFactory.externalTransactionManagementAdapter != None) {
      val s = SessionFactory.externalTransactionManagementAdapter.get.apply
      s.bindToCurrentThread
      s
    }
    else _currentSessionThreadLocal.get.getOrElse(
      error("no session is bound to current thread, a session must be created via Session.create \nand bound to the thread via 'work' or 'bindToCurrentThread'"))

  def hasCurrentSession =
    _currentSessionThreadLocal.get != None

  def cleanupResources =
    if(_currentSessionThreadLocal != None)
      _currentSessionThreadLocal.get.get.cleanup

  private def currentSession_=(s: Option[Session]) = _currentSessionThreadLocal.set(s)
}
