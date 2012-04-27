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
package org.squeryl.customtypes;


import java.util.{Date, UUID}
import org.squeryl.dsl._
import java.sql.Timestamp
import org.squeryl.internals.{OutMapper, FieldReferenceLinker, FieldMapper}
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.IntType
import java.sql.ResultSet

trait CustomType[T] extends Product1[T] {
  def value: T
  def _1 = value
  def canEqual(a:Any) = false
}

trait CustomTypesMode extends QueryDsl with FieldMapper {
  
  private val ps = PrimitiveTypeSupport
    
  val stringTEF = new NonPrimitiveJdbcMapper[String,StringField,TString](ps.stringTEF, this) {
    def convertFromJdbc(v: String) = new StringField(v)
    def convertToJdbc(v: StringField) = v.value
  }
        
  val optionStringTEF = new TypedExpressionFactory[Option[StringField],TOptionString] with DeOptionizer[StringField, TString, Option[StringField], TOptionString]{
    val deOptionizer = stringTEF
  }
    
  val dateTEF = new NonPrimitiveJdbcMapper[Date, DateField,TDate](ps.dateTEF, this) {
    def convertFromJdbc(v: Date) = new DateField(v)
    def convertToJdbc(v: DateField) = v.value
  }
    
  val optionDateTEF = new TypedExpressionFactory[Option[DateField],TOptionDate] with DeOptionizer[DateField, TDate, Option[DateField], TOptionDate] {    
    val deOptionizer = dateTEF
  }

  val timestampTEF = new NonPrimitiveJdbcMapper[Timestamp, TimestampField,TTimestamp](ps.timestampTEF, this) {
    def convertFromJdbc(v: Timestamp) = new TimestampField(v)
    def convertToJdbc(v: TimestampField) = v.value

  }
  
  val optionTimestampTEF = new TypedExpressionFactory[Option[TimestampField],TOptionTimestamp] with DeOptionizer[TimestampField, TTimestamp, Option[TimestampField], TOptionTimestamp] {
    val deOptionizer = timestampTEF
  }

  val booleanTEF = new NonPrimitiveJdbcMapper[Boolean, BooleanField,TBoolean](ps.booleanTEF, this) {
    def convertFromJdbc(v: Boolean) = new BooleanField(v)
    def convertToJdbc(v: BooleanField) = v.value
  }
    
  val optionBooleanTEF = new TypedExpressionFactory[Option[BooleanField],TOptionBoolean] with DeOptionizer[BooleanField, TBoolean, Option[BooleanField], TOptionBoolean] {
    val deOptionizer = booleanTEF
  }
  
  val uuidTEF = new NonPrimitiveJdbcMapper[UUID,UuidField,TUUID](ps.uuidTEF, this) {
    def convertFromJdbc(v: UUID) = new UuidField(v)
    def convertToJdbc(v: UuidField) = v.value
  }
        
  val optionUUIDTEF = new TypedExpressionFactory[Option[UuidField],TOptionUUID] with DeOptionizer[UuidField, TUUID, Option[UuidField], TOptionUUID] {
    val deOptionizer = uuidTEF
  }
    
    // =========================== Numerical Integral =========================== 
  
  val byteTEF = new NonPrimitiveJdbcMapper[Byte, ByteField,TByte](ps.byteTEF, this) {
    def convertFromJdbc(v: Byte) = new ByteField(v)
    def convertToJdbc(v: ByteField) = v.value
  }
    
  val optionByteTEF = new IntegralTypedExpressionFactory[Option[ByteField],TOptionByte, Option[FloatField], TOptionFloat] with DeOptionizer[ByteField, TByte, Option[ByteField], TOptionByte] {
    val deOptionizer = byteTEF
    val floatifyer = optionFloatTEF
  }

  val intTEF = new NonPrimitiveJdbcMapper[Int, IntField,TInt](ps.intTEF, this) with IntegralTypedExpressionFactory[IntField,TInt,FloatField,TFloat] {
    val floatifyer = floatTEF
    def convertFromJdbc(v: Int) = new IntField(v)
    def convertToJdbc(v: IntField) = v.value
  }  
    
  val optionIntTEF = new IntegralTypedExpressionFactory[Option[IntField],TOptionInt,Option[FloatField],TOptionFloat] with DeOptionizer[IntField,TInt,Option[IntField],TOptionInt] {
    val deOptionizer = intTEF
    val floatifyer = optionFloatTEF
  }
    
  val longTEF = new NonPrimitiveJdbcMapper[Long, LongField,TLong](ps.longTEF, this) with IntegralTypedExpressionFactory[LongField,TLong,DoubleField,TDouble] {
    val floatifyer = doubleTEF      
    def convertFromJdbc(v: Long) = new LongField(v)
    def convertToJdbc(v: LongField) = v.value
  }
  
  val optionLongTEF = new IntegralTypedExpressionFactory[Option[LongField],TOptionLong,Option[DoubleField],TOptionDouble] with DeOptionizer[LongField,TLong,Option[LongField],TOptionLong] {
    val deOptionizer = longTEF
    val floatifyer = optionDoubleTEF
  }
    
  // =========================== Numerical Floating Point =========================== 
    
  val floatTEF = new NonPrimitiveJdbcMapper[Float, FloatField,TFloat](ps.floatTEF, this) with FloatTypedExpressionFactory[FloatField,TFloat] {
    def convertFromJdbc(v: Float) = new FloatField(v)
    def convertToJdbc(v: FloatField) = v.value
  }
    
  val optionFloatTEF = new FloatTypedExpressionFactory[Option[FloatField],TOptionFloat] with DeOptionizer[FloatField,TFloat,Option[FloatField],TOptionFloat] {
    val deOptionizer = floatTEF
  }
    
  val doubleTEF = new NonPrimitiveJdbcMapper[Double, DoubleField,TDouble](ps.doubleTEF, this) with FloatTypedExpressionFactory[DoubleField,TDouble] { 
    def convertFromJdbc(v: Double) = new DoubleField(v)
    def convertToJdbc(v: DoubleField) = v.value
  }
    
  val optionDoubleTEF = new FloatTypedExpressionFactory[Option[DoubleField],TOptionDouble] with DeOptionizer[DoubleField,TDouble,Option[DoubleField],TOptionDouble] {
    val deOptionizer = doubleTEF
  }
    
  val bigDecimalTEF = new NonPrimitiveJdbcMapper[BigDecimal, BigDecimalField,TBigDecimal](ps.bigDecimalTEF, this) with FloatTypedExpressionFactory[BigDecimalField,TBigDecimal] {
    def convertFromJdbc(v: BigDecimal) = new BigDecimalField(v)
    def convertToJdbc(v: BigDecimalField) = v.value
  }
    
  val optionBigDecimalTEF = new FloatTypedExpressionFactory[Option[BigDecimalField],TOptionBigDecimal] with DeOptionizer[BigDecimalField,TBigDecimal,Option[BigDecimalField],TOptionBigDecimal] {
    val deOptionizer = bigDecimalTEF
  }      
  
  
  implicit def stringToTE(s: String) = stringTEF.createFromNativeJdbcValue(s)  
  implicit def optionStringToTE(s: Option[String]) = s.map(new StringField(_))
  
  
  implicit def dateToTE(s: Date) = dateTEF.createFromNativeJdbcValue(s)
  implicit def optionDateToTE(s: Option[Date]) = s.map(new DateField(_))
  
  implicit def timestampToTE(s: Timestamp) = timestampTEF.createFromNativeJdbcValue(s)    
  implicit def optionTimestampToTE(s: Option[Timestamp]) = s.map(new TimestampField(_))
  
  implicit def booleanToTE(s: Boolean) = booleanTEF.createFromNativeJdbcValue(s)
  implicit def optionBooleanToTE(s: Option[Boolean]) = s.map(new BooleanField(_))
  
  implicit def uuidToTE(s: UUID) = uuidTEF.createFromNativeJdbcValue(s)
  implicit def optionUUIDToTE(s: Option[UUID]) = s.map(new UuidField(_))
  
  
  implicit def byteToTE(f: Byte) = byteTEF.createFromNativeJdbcValue(f)    
  implicit def optionByteToTE(f: Option[Byte]) = f.map(new ByteField(_))

  implicit def intToTE(f: IntField) = intTEF.create(f)
  implicit def optionIntToTE(f: Option[IntField]) = optionIntTEF.create(f)
  
  //implicit def _intToTE(f: Int) = intTEF.createFromNativeJdbcValue(f)
  //implicit def _optionIntToTE(f: Option[Int]) = f.map(new IntField(_))
  
  implicit def longToTE(f: Long) = longTEF.createFromNativeJdbcValue(f)
  implicit def optionLongToTE(f: Option[Long]) = f.map(new LongField(_))
  
  implicit def floatToTE(f: Float) = floatTEF.createFromNativeJdbcValue(f)
  implicit def optionFloatToTE(f: Option[Float]) = f.map(new FloatField(_))
  
  implicit def doubleToTE(f: Double) = doubleTEF.createFromNativeJdbcValue(f)
  implicit def optionDoubleToTE(f: Option[Double]) = f.map(new DoubleField(_))
  
  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.createFromNativeJdbcValue(f)
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = f.map(new BigDecimalField(_))
}

object CustomTypesMode extends CustomTypesMode 


class ByteField(val value: Byte) extends CustomType[Byte]

class IntField(val value: Int) extends CustomType[Int]

class StringField(val value: String) extends CustomType[String]

class DoubleField(val value: Double) extends CustomType[Double]

class BigDecimalField(val value: BigDecimal) extends CustomType[BigDecimal]

class FloatField(val value: Float) extends CustomType[Float]

class LongField(val value: Long) extends CustomType[Long]

class BooleanField(val value: Boolean) extends CustomType[Boolean]

class DateField(val value: Date) extends CustomType[Date]

class TimestampField(val value: Timestamp) extends CustomType[Timestamp]

class BinaryField(val value: Array[Byte]) extends CustomType[Array[Byte]]

class UuidField(val value: UUID) extends CustomType[UUID]

