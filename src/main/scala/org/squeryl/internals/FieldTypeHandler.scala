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


trait FieldTypeHandler[T] {

  private def isInt(t: Class[_]) = t.isAssignableFrom(classOf[Int]) || t.isAssignableFrom(classOf[java.lang.Integer])
  private def isLong(t: Class[_]) = t.isAssignableFrom(classOf[Long]) || t.isAssignableFrom(classOf[java.lang.Long])
  private def isString(t: Class[_]) = t.isAssignableFrom(classOf[java.lang.String])
  private def isBoolean(t: Class[_]) = t.isAssignableFrom(classOf[Boolean]) || t.isAssignableFrom(classOf[java.lang.Boolean])
  private def isDouble(t: Class[_]) = t.isAssignableFrom(classOf[Double]) || t.isAssignableFrom(classOf[java.lang.Double])
  private def isFloat(t: Class[_]) = t.isAssignableFrom(classOf[Float]) || t.isAssignableFrom(classOf[java.lang.Float])
  private def isDate(t: Class[_]) = classOf[java.util.Date].isAssignableFrom(t)
  private def isBigDecimal(t: Class[_]) = t.isAssignableFrom(classOf[scala.math.BigDecimal]) || t.isAssignableFrom(classOf[java.math.BigDecimal])
  private def isTimestamp(t: Class[_]) = classOf[java.sql.Timestamp].isAssignableFrom(t)
  private def isBinary(t: Class[_]) = t.isAssignableFrom(classOf[Array[Byte]])
  private def isEnumerationValueType(t: Class[_]) = classOf[Enumeration#Value].isAssignableFrom(t)

  def handleType(t: Class[_], fmd: Option[FieldMetaData]) = {

      if(isBigDecimal(t))
        handleBigDecimalType(fmd)
      else if(isInt(t))
        handleIntType
      else if(isLong(t))
        handleLongType
      else if(isString(t))
        handleStringType(fmd)
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
      else
        handleUnknownType(t)
  }

  protected def handleIntType : T
  protected def handleStringType : T
  protected def handleStringType(fmd: Option[FieldMetaData]) : T
  protected def handleBooleanType : T
  protected def handleDoubleType : T
  protected def handleDateType: T
  protected def handleLongType: T
  protected def handleFloatType: T
  protected def handleBigDecimalType(fmd: Option[FieldMetaData]): T
  protected def handleTimestampType: T
  protected def handleBinaryType: T
  protected def handleEnumerationValueType: T

  protected def handleUnknownType(c: Class[_]) : T
}
