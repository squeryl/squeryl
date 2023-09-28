package org.squeryl.internals

import org.squeryl.Session
import org.squeryl.dsl.{ArrayJdbcMapper, TypedExpressionFactory}

import java.sql
import java.sql.ResultSet

abstract class ArrayTEF[P, TE] extends TypedExpressionFactory[Array[P], TE] with ArrayJdbcMapper[java.sql.Array, Array[P]] {
  // must define "sample" that includes an element. e.g. Array[Int](0)
  def sample: Array[P]

  def toWrappedJDBCType(element: P): java.lang.Object

  def fromWrappedJDBCType(element: Array[java.lang.Object]): Array[P]

  val defaultColumnLength = 1

  def extractNativeJdbcValue(rs: ResultSet, i: Int): sql.Array = rs.getArray(i)

  def convertToJdbc(v: Array[P]): java.sql.Array = {
    val content: Array[java.lang.Object] = v.map(toWrappedJDBCType)
    val s = Session.currentSession
    val con = s.connection
    var rv: java.sql.Array = null
    try {
      val typ = s.databaseAdapter.arrayCreationType(toWrappedJDBCType(sample(0)).getClass)
      rv = con.createArrayOf(typ, content)
    } catch {
      case e: Exception => s.log("Cannot create JDBC array: " + e.getMessage)
    }
    rv
  }

  def convertFromJdbc(v: java.sql.Array): Array[P] = {
    val s = Session.currentSession
    var rv: Array[P] = sample.take(0)
    try {
      val obj = v.getArray();
      rv = fromWrappedJDBCType(obj.asInstanceOf[Array[java.lang.Object]])
    } catch {
      case e: Exception => s.log("Cannot obtain array from JDBC: " + e.getMessage)
    }
    rv
  }

}