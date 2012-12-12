package org.squeryl.internals

import java.util.Map
import java.sql.Array
import java.sql.ResultSet

class SampleSQLArray extends Array {

  def getBaseTypeName(): String = { null }

  def getBaseType(): Int = { 0 }

  def getArray(): Object = { null }

  def getArray(arg0: java.util.Map[java.lang.String,java.lang.Class[_]]): Object = { null }

  def getArray(arg0: Long, arg1: Int): Object = { null }

  def getArray(arg0: Long, arg1: Int, arg2: java.util.Map[java.lang.String,java.lang.Class[_]]): Object = { null }

  def getResultSet(): ResultSet = { null }

  def getResultSet(arg0: java.util.Map[java.lang.String,java.lang.Class[_]]): ResultSet = { null }

  def getResultSet(arg0: Long, arg1: Int): ResultSet = { null }

  def getResultSet(arg0: Long, arg1: Int, arg2: java.util.Map[java.lang.String,java.lang.Class[_]]): ResultSet = { null }

  def free(): Unit = {}

}