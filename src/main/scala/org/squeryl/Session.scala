package org.squeryl

import org.squeryl.internals._;
import java.sql.Connection;

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

  def work[A](a: =>A): A = {
    bindToCurrentThread
    val r = a
    unbindFromCurrentThread
    r
  }
}

object Session {

  private val _currentSessionThreadLocal = new ThreadLocal[Option[Session]] {
    override def initialValue = None
  }
  
  def create(c: Connection, a: DatabaseAdapter) = new Session {
    def connection = c
    def databaseAdapter = a
  }

  def currentSession =
    _currentSessionThreadLocal.get.getOrElse(
      error("no session is bound to current thread, a session must be created via Session.create \nand bound to the thread via 'work' or 'bindToCurrentThread'"))

  def hasCurrentSession =
    _currentSessionThreadLocal.get != None

  private def currentSession_=(s: Option[Session]) = _currentSessionThreadLocal.set(s)
}
