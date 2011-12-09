/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.internals
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object Zaza {
  
  def main(args: Array[String]) {
    println(classOf[Int].getName)
    println(classOf[Int].getCanonicalName)
  } 
}


class FieldMapperz {

  def nativeJdbcTypeFor(c: Class[_]): Class[_] = null
}


trait FieldTypeHandler[T] {

  private def isInt(t: Class[_]) = classOf[Int].isAssignableFrom(t) || classOf[java.lang.Integer].isAssignableFrom(t)
  private def isLong(t: Class[_]) = classOf[Long].isAssignableFrom(t) || classOf[java.lang.Long].isAssignableFrom(t)
  private def isBoolean(t: Class[_]) = classOf[Boolean].isAssignableFrom(t) || classOf[java.lang.Boolean].isAssignableFrom(t)
  private def isDouble(t: Class[_]) = classOf[Double].isAssignableFrom(t) || classOf[java.lang.Double].isAssignableFrom(t)
  private def isFloat(t: Class[_]) = classOf[Float].isAssignableFrom(t) || classOf[java.lang.Float].isAssignableFrom(t)
  
  private def isString(t: Class[_]) = classOf[java.lang.String].isAssignableFrom(t)
  private def isDate(t: Class[_]) = classOf[java.util.Date].isAssignableFrom(t)
  private def isBigDecimal(t: Class[_]) = classOf[scala.math.BigDecimal].isAssignableFrom(t)
  private def isTimestamp(t: Class[_]) = classOf[java.sql.Timestamp].isAssignableFrom(t)
  private def isBinary(t: Class[_]) = classOf[Array[Byte]].isAssignableFrom(t)
  private def isEnumerationValueType(t: Class[_]) = classOf[Enumeration#Value].isAssignableFrom(t)
  private def isUuid(t: Class[_]) = classOf[java.util.UUID].isAssignableFrom(t)
  
  def handleType(t: Class[_]) = {

      if(isBigDecimal(t))
        handleBigDecimalType
      else if(isInt(t))
        handleIntType
      else if(isLong(t))
        handleLongType
      else if(isString(t))
        handleStringType
      else if(isBoolean(t))
        handleBooleanType
      else if(isDouble(t))
        handleDoubleType
      else if(isFloat(t))  
        handleFloatType
      else if(isTimestamp(t))
        handleTimestampType
      else if(isDate(t))
        handleDateType
      else if(isBinary(t))
        handleBinaryType
      else if(isEnumerationValueType(t))
        handleEnumerationValueType
      else if (isUuid(t))
        handleUuidType
      else
        handleUnknownType(t)
  }

  protected def handleIntType : T
  protected def handleStringType : T
  protected def handleBooleanType : T
  protected def handleDoubleType : T
  protected def handleDateType: T
  protected def handleLongType: T
  protected def handleFloatType: T
  protected def handleBigDecimalType: T
  protected def handleTimestampType: T
  protected def handleBinaryType: T
  protected def handleEnumerationValueType: T
  protected def handleUuidType: T

  protected def handleUnknownType(c: Class[_]) : T
}
