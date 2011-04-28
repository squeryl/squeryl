package org.squeryl.test

import org.squeryl.framework.DbTestBase
import org.squeryl.{SessionFactory, Session}

abstract class ConnectionClosingTest extends DbTestBase {

  test("Closing statement should close connection"){
    val session = SessionFactory.newSession
    val stmt = session.connection.prepareStatement("select now()")
    val rs = stmt.executeQuery

    session.connection.close

    stmt.isClosed should equal(false)
    rs.isClosed should equal(false)
  }
}