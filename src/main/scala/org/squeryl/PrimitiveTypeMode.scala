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

/**
 *  This factory is meant to use POSOs (Plain old Scala Objects),
 * i.e. your object use Scala's primitive types to map to columns.
 * This can have a significant performance advantage over using object
 * types i.e. a result set of N rows of objects with M field will generate
 * N * M objects for the garbage collector, while POSOs with primitive types
 * will each count for 1 for the garbage collector (to be more precise,
 * String and Option[] fields will add a +1 in both cases, but a custom String wrapper will
 * also add one ref, for a total of 2 refs vs a single ref per string column
 * for the POSO).
 *  This lightweight strategy has a cost : constants and object field references
 * cannot distinguish at compile time, so this mode is less 'strict' than
 * one with a CustomType 
 */

object PrimitiveTypeMode extends PrimitiveTypeMode

trait PrimitiveTypeMode extends QueryDsl {

  // =========================== Non Numerical =========================== 
  
  implicit val stringTEF = new TypedExpressionFactory[String,TString] {
    //def create(v: String) = new ConstantTypedExpression[String,TString](v)
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[String,TString](v,this)
    def sample = "": String
    def doMap(rs: ResultSet, i: Int) = rs.getString(i)
  }
    
  implicit def stringToTE(s: String) = stringTEF.create(s)
  
  implicit val optionStringTEF = new TypedExpressionFactory[Option[String],TOptionString] with DeOptionizer[String, TString, Option[String], TOptionString]{
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[String],TOptionString](v,this)
    def sample = Option("")
    def deOptionizer = stringTEF
  }
  
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)  

  implicit val dateTEF = new TypedExpressionFactory[Date,TDate] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Date,TDate](v,this)
    def sample = new Date
    def doMap(rs: ResultSet, i: Int) = rs.getDate(i)
  }
  
  implicit def dateToTE(s: Date) = dateTEF.create(s)  

  implicit val optionDateTEF = new TypedExpressionFactory[Option[Date],TOptionDate] with DeOptionizer[Date, TDate, Option[Date], TOptionDate] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Date],TOptionDate](v,this)
    def sample = Option(new Date)
    def deOptionizer = dateTEF
  }
  
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)    
  
  
  implicit val timestampTEF = new TypedExpressionFactory[Timestamp,TTimestamp] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Timestamp,TTimestamp](v,this)
    val sample = new Timestamp(0)
    def doMap(rs: ResultSet, i: Int) = rs.getTimestamp(i)
  }
  
  implicit def timestampToTE(s: Timestamp) = timestampTEF.create(s)  

  implicit val optionTimestampTEF = new TypedExpressionFactory[Option[Timestamp],TOptionTimestamp] with DeOptionizer[Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Timestamp],TOptionTimestamp](v,this)
    val sample = Option(new Timestamp(0))
    def deOptionizer = timestampTEF
  }
  
  implicit def optionTimestampToTE(s: Option[Timestamp]) = optionTimestampTEF.create(s)    
  
  implicit val booleanTEF = new TypedExpressionFactory[Boolean,TBoolean] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Boolean,TBoolean](v,this)
    def sample = true
    def doMap(rs: ResultSet, i: Int) = rs.getBoolean(i)
  }
    
  implicit def booleanToTE(s: Boolean) = booleanTEF.create(s)
  
  implicit val optionBooleanTEF = new TypedExpressionFactory[Option[Boolean],TOptionBoolean] with DeOptionizer[Boolean, TBoolean, Option[Boolean], TOptionBoolean] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Boolean],TOptionBoolean](v,this)
    def sample = Option(true)
    def deOptionizer = booleanTEF
  }
  
  implicit def optionBooleanToTE(s: Option[Boolean]) = optionBooleanTEF.create(s)  

  implicit val uuidTEF = new TypedExpressionFactory[UUID,TUUID] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[UUID,TUUID](v,this)
    def sample = UUID.randomUUID
    def doMap(rs: ResultSet, i: Int) = {
      val v = rs.getObject(i)
      v match {
          case u: UUID => u
          case s: String => UUID.fromString(s)
      }
    }
  }
    
  implicit def uuidToTE(s: UUID) = uuidTEF.create(s)
  
  implicit val optionUUIDTEF = new TypedExpressionFactory[Option[UUID],TOptionUUID] with DeOptionizer[UUID, TUUID, Option[UUID], TOptionUUID] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[UUID],TOptionUUID](v,this)
    def sample = Option(UUID.randomUUID)
    def deOptionizer = uuidTEF
  }
  
  implicit def optionUUIDToTE(s: Option[UUID]) = optionUUIDTEF.create(s)  
  
  implicit val binaryTEF = new TypedExpressionFactory[Array[Byte],TByteArray] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Array[Byte],TByteArray](v,this)
    def sample = Array(0: Byte)   
    def doMap(rs: ResultSet, i: Int) = rs.getBytes(i)
  }
    
  implicit def binaryToTE(s: Array[Byte]) = binaryTEF.create(s)
  
  implicit val optionByteArrayTEF = new TypedExpressionFactory[Option[Array[Byte]],TOptionByteArray] with DeOptionizer[Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Array[Byte]],TOptionByteArray](v,this)
    def sample = Option(Array(0: Byte))
    def deOptionizer = binaryTEF
  }
  
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]) = optionByteArrayTEF.create(s)  
  
  def enumValueTEF[A <: Enumeration#Value](ev: Enumeration#Value) = new TypedExpressionFactory[A,TEnumValue[A]] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[A,TEnumValue[A]](v,this)
    def sample: A = sys.error("!")
    
    val enu = Utils.enumerationForValue(ev)
    
    def doMap(rs: ResultSet, i: Int) = {
      val enumIdx = rs.getInt(i)      
      enu.values.find(_.id == enumIdx).get.asInstanceOf[A]
    }
  }
  
  implicit def enumValueToTE[A <: Enumeration#Value](e: A) = enumValueTEF(e).create(e)
  
  implicit def optionEnumValueTEF[A <: Enumeration#Value](ev: Enumeration#Value) = new TypedExpressionFactory[Option[A],TOptionEnumValue[A]] with DeOptionizer[A,TEnumValue[A],Option[A],TOptionEnumValue[A]] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[A],TOptionEnumValue[A]](v,this)
    def sample: Option[A] = sys.error("!")
    def deOptionizer = enumValueTEF[A](ev)
  }

  //TODO: test where(anEnum === None)
  implicit def optionEnumcValueToTE[A <: Enumeration#Value](e: Option[A]) = optionEnumValueTEF(e.get).create(e)
  
  // =========================== Numerical Integral =========================== 

  implicit val byteTEF = new IntegralTypedExpressionFactory[Byte,TByte,Float,TFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Byte,TByte](v,this)
    def sample = 1: Byte
    def floatifyer = floatTEF
    def doMap(rs: ResultSet, i: Int) = rs.getByte(i)
  }
  
  implicit def byteToTE(f: Byte) = byteTEF.create(f)  

  implicit val optionByteTEF = new IntegralTypedExpressionFactory[Option[Byte],TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, TByte, Option[Byte], TOptionByte] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Byte],TOptionByte](v,this)
    def deOptionizer = byteTEF
    def sample = Option(0 : Byte)
    def floatifyer = optionFloatTEF
  }
  
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)  
    
  implicit val intTEF = new IntegralTypedExpressionFactory[Int,TInt,Float,TFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Int,TInt](v,this)
    def sample = 1
    def floatifyer = floatTEF
    def doMap(rs: ResultSet, i: Int) = rs.getInt(i)
  }  
    
  implicit def intToTE(f: Int) = intTEF.create(f)  

  implicit val optionIntTEF = new IntegralTypedExpressionFactory[Option[Int],TOptionInt,Option[Float],TOptionFloat] with DeOptionizer[Int,TInt,Option[Int],TOptionInt] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Int],TOptionInt](v,this)
    def deOptionizer = intTEF
    def sample = Option(0)
    def floatifyer = optionFloatTEF
  }

  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)
  
  implicit val longTEF = new IntegralTypedExpressionFactory[Long,TLong,Double,TDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Long,TLong](v,this)
    def sample = 1L
    def floatifyer = doubleTEF
    def doMap(rs: ResultSet, i: Int) = rs.getLong(i)
  }
  
  implicit def longToTE(f: Long) = longTEF.create(f)

  implicit val optionLongTEF = new IntegralTypedExpressionFactory[Option[Long],TOptionLong,Option[Double],TOptionDouble] with DeOptionizer[Long,TLong,Option[Long],TOptionLong] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Long],TOptionLong](v,this)
    def deOptionizer = longTEF
    def sample = Option(0L)
    def floatifyer = optionDoubleTEF
  }
  
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)  
  
  // =========================== Numerical Floating Point =========================== 
  
  implicit val floatTEF = new FloatTypedExpressionFactory[Float,TFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Float,TFloat](v,this)
    def sample = 1F
    def doMap(rs: ResultSet, i: Int) = rs.getFloat(i)
  }
    
  implicit def floatToTE(f: Float) = floatTEF.create(f)
  
  implicit val optionFloatTEF = new FloatTypedExpressionFactory[Option[Float],TOptionFloat] with DeOptionizer[Float,TFloat,Option[Float],TOptionFloat] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Float],TOptionFloat](v,this)
    def deOptionizer = floatTEF
    def sample = Option(0F)
  }

  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)
  
  implicit val doubleTEF = new FloatTypedExpressionFactory[Double,TDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Double,TDouble](v,this)
    def sample = 1D
    def doMap(rs: ResultSet, i: Int) = rs.getDouble(i)
  }

  implicit def doubleToTE(f: Double) = doubleTEF.create(f)    
  
  implicit val optionDoubleTEF = new FloatTypedExpressionFactory[Option[Double],TOptionDouble] with DeOptionizer[Double,TDouble,Option[Double],TOptionDouble] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[Double],TOptionDouble](v,this)
    def deOptionizer = doubleTEF
    def sample = Option(0D)
  }
  
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)
  
  implicit val bigDecimalTEF = new FloatTypedExpressionFactory[BigDecimal,TBigDecimal] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[BigDecimal,TBigDecimal](v,this)
    def sample = BigDecimal(1)
    def doMap(rs: ResultSet, i: Int) = rs.getBigDecimal(i)
  }

  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)
  
  implicit val optionBigDecimalTEF = new FloatTypedExpressionFactory[Option[BigDecimal],TOptionBigDecimal] with DeOptionizer[BigDecimal,TBigDecimal,Option[BigDecimal],TOptionBigDecimal] {
    def convert(v: TypedExpression[_,_]) = new TypedExpressionConversion[Option[BigDecimal],TOptionBigDecimal](v,this)
    def deOptionizer = bigDecimalTEF
    def sample = Option(BigDecimal(0))
  }
    
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)
  
}

object DummyEnum extends Enumeration {
  type DummyEnum = Value
  val DummyEnumerationValue = Value(-1, "DummyEnumerationValue")
}
