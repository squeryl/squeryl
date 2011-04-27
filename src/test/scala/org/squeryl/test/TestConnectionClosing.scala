package org.squeryl.test

import org.squeryl.framework.DbTestBase
import org.squeryl.{SessionFactory, Session}

abstract class ConnectionClosingTest extends DbTestBase {

  test("Closing statement should close connection"){
    val session = SessionFactory.newSession
    val stmt = session.connection.prepareStatement("select now()")
    val rs = stmt.executeQuery

    session.connection.close

    println("When " + session.connection.getClass.getName + " closes :")
    println("statement is closed ? --> " + stmt.isClosed)
    println("resultSet is closed ? --> " + rs.isClosed)

    stmt.isClosed should equal(false)
    rs.isClosed should equal(false)
  }
}