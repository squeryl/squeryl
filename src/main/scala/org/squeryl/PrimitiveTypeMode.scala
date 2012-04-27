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
package org.squeryl


import dsl.ast._
import dsl._
import internals.FieldReferenceLinker
import java.util.{ Date, UUID }
import java.sql.Timestamp
import java.sql.ResultSet
import org.squeryl.internals.Utils
import org.squeryl.internals.FieldMapper


object PrimitiveTypeMode extends PrimitiveTypeMode

trait PrimitiveTypeMode extends QueryDsl with FieldMapper {
    
  
  // =========================== Non Numerical =========================== 
  implicit val stringTEF = PrimitiveTypeSupport.stringTEF
  implicit val optionStringTEF = PrimitiveTypeSupport.optionStringTEF
  implicit val dateTEF = PrimitiveTypeSupport.dateTEF
  implicit val optionDateTEF = PrimitiveTypeSupport.optionDateTEF    
  implicit val timestampTEF = PrimitiveTypeSupport.timestampTEF
  implicit val optionTimestampTEF = PrimitiveTypeSupport.optionTimestampTEF
  
  // =========================== Numerical Integral =========================== 
  implicit val byteTEF = PrimitiveTypeSupport.byteTEF
  implicit val optionByteTEF = PrimitiveTypeSupport.optionByteTEF
  implicit val intTEF = PrimitiveTypeSupport.intTEF
  implicit val optionIntTEF = PrimitiveTypeSupport.optionIntTEF
  implicit val longTEF = PrimitiveTypeSupport.longTEF
  implicit val optionLongTEF = PrimitiveTypeSupport.optionLongTEF
  
  // =========================== Numerical Floating Point ===========================   
  implicit val floatTEF = PrimitiveTypeSupport.floatTEF
  implicit val optionFloatTEF = PrimitiveTypeSupport.optionFloatTEF
  implicit val doubleTEF = PrimitiveTypeSupport.doubleTEF
  implicit val optionDoubleTEF = PrimitiveTypeSupport.optionDoubleTEF  
  implicit val bigDecimalTEF = PrimitiveTypeSupport.bigDecimalTEF
  implicit val optionBigDecimalTEF = PrimitiveTypeSupport.optionBigDecimalTEF
  
  
  implicit def stringToTE(s: String) = stringTEF.create(s)  
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)
  
  implicit def dateToTE(s: Date) = dateTEF.create(s)    
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)
  
  implicit def timestampToTE(s: Timestamp) = timestampTEF.create(s)    
  implicit def optionTimestampToTE(s: Option[Timestamp]) = optionTimestampTEF.create(s)
  
  implicit def booleanToTE(s: Boolean) = PrimitiveTypeSupport.booleanTEF.create(s)  
  implicit def optionBooleanToTE(s: Option[Boolean]) = PrimitiveTypeSupport.optionBooleanTEF.create(s)
  
  implicit def uuidToTE(s: UUID) = PrimitiveTypeSupport.uuidTEF.create(s)  
  implicit def optionUUIDToTE(s: Option[UUID]) = PrimitiveTypeSupport.optionUUIDTEF.create(s)
  
  implicit def binaryToTE(s: Array[Byte]) = PrimitiveTypeSupport.binaryTEF.create(s)  
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]) = PrimitiveTypeSupport.optionByteArrayTEF.create(s)
  
  implicit def enumValueToTE[A <: Enumeration#Value](e: A) = 
    PrimitiveTypeSupport.enumValueTEF(e).create(e)
    
  implicit def optionEnumcValueToTE[A <: Enumeration#Value](e: Option[A]) = 
    PrimitiveTypeSupport.optionEnumValueTEF(e).create(e)
  
  implicit def byteToTE(f: Byte) = byteTEF.create(f)    
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)
  
  implicit def intToTE(f: Int) = intTEF.create(f)  
  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)
  
  implicit def longToTE(f: Long) = longTEF.create(f)  
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)
  
  implicit def floatToTE(f: Float) = floatTEF.create(f)
  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)
  
  implicit def doubleToTE(f: Double) = doubleTEF.create(f)      
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)
  
  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)    
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)
  
}
