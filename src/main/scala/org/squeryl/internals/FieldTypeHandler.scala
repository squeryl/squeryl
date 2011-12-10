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

/*
package org.squeryl.internals
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.sql.ResultSet
import java.sql.Timestamp
import java.util.Date
import java.util.UUID
import org.squeryl.dsl._


trait FieldMapperz {

  private val registry = new HashMap[Class[_],FieldAttributesBasedOnType[_]]

  implicit def thisFieldMapperz = this

  // =========================== Non Numerical =========================== 
  
  implicit val stringTEF = new TypedExpressionFactory[String,TString] with PrimitiveJdbcMapper[String] {
    val sample = "": String
    val defaultColumnLength = 128
    def doMap(rs: ResultSet, i: Int) = rs.getString(i)
  }
    
  implicit def stringToTE(s: String) = stringTEF.create(s)
  
  implicit val optionStringTEF = new TypedExpressionFactory[Option[String],TOptionString] with DeOptionizer[String, TString, Option[String], TOptionString]{
    val deOptionizer = stringTEF
  }
  
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)  

  implicit val dateTEF = new TypedExpressionFactory[Date,TDate] with PrimitiveJdbcMapper[Date] {
    val sample = new Date
    val defaultColumnLength = -1
    def doMap(rs: ResultSet, i: Int) = rs.getDate(i)
  }
  
  implicit def dateToTE(s: Date) = dateTEF.create(s)  

  implicit val optionDateTEF = new TypedExpressionFactory[Option[Date],TOptionDate] with DeOptionizer[Date, TDate, Option[Date], TOptionDate] {    
    val deOptionizer = dateTEF
  }
  
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)    
  
  
  implicit val timestampTEF = new TypedExpressionFactory[Timestamp,TTimestamp] with PrimitiveJdbcMapper[Timestamp] {
    val sample = new Timestamp(0)
    val defaultColumnLength = -1
    def doMap(rs: ResultSet, i: Int) = rs.getTimestamp(i)
  }
  
  implicit def timestampToTE(s: Timestamp) = timestampTEF.create(s)  

  implicit val optionTimestampTEF = new TypedExpressionFactory[Option[Timestamp],TOptionTimestamp] with DeOptionizer[Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] {
    val deOptionizer = timestampTEF
  }
  
  implicit def optionTimestampToTE(s: Option[Timestamp]) = optionTimestampTEF.create(s)    
  
  implicit val booleanTEF = new TypedExpressionFactory[Boolean,TBoolean] with PrimitiveJdbcMapper[Boolean] {
    val sample = true
    val defaultColumnLength = 1
    def doMap(rs: ResultSet, i: Int) = rs.getBoolean(i)
  }
    
  implicit def booleanToTE(s: Boolean) = booleanTEF.create(s)
  
  implicit val optionBooleanTEF = new TypedExpressionFactory[Option[Boolean],TOptionBoolean] with DeOptionizer[Boolean, TBoolean, Option[Boolean], TOptionBoolean] {
    val deOptionizer = booleanTEF
  }
  
  implicit def optionBooleanToTE(s: Option[Boolean]) = optionBooleanTEF.create(s)  

  implicit val uuidTEF = new TypedExpressionFactory[UUID,TUUID] with PrimitiveJdbcMapper[UUID] {
    val sample = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
    val defaultColumnLength = 36 
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
    val deOptionizer = uuidTEF
  }
  
  implicit def optionUUIDToTE(s: Option[UUID]) = optionUUIDTEF.create(s)  
  
  implicit val binaryTEF = new TypedExpressionFactory[Array[Byte],TByteArray] with PrimitiveJdbcMapper[Array[Byte]] {
    val sample = Array(0: Byte)   
    val defaultColumnLength = 255
    def doMap(rs: ResultSet, i: Int) = rs.getBytes(i)
  }
    
  implicit def binaryToTE(s: Array[Byte]) = binaryTEF.create(s)
  
  implicit val optionByteArrayTEF = new TypedExpressionFactory[Option[Array[Byte]],TOptionByteArray] with DeOptionizer[Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] {
    val deOptionizer = binaryTEF
  }
  
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]) = optionByteArrayTEF.create(s)  
  
  def enumValueTEF[A <: Enumeration#Value](ev: Enumeration#Value) = new NonPrimitiveJdbcMapper[Int,A,TEnumValue[A]]()(intTEF, this) {
    val enu = Utils.enumerationForValue(ev)
    //TODO: avoid isInstanceOf
    override def sample: A = ev.asInstanceOf[A]

    def convertToJdbc(v: A) = (v : Enumeration#Value).id       
      
    def convertFromJdbc(v: Int): A = 
      enu.values.find(_.id == v).get.asInstanceOf[A]
  }
  
  implicit def enumValueToTE[A <: Enumeration#Value](e: A) = enumValueTEF(e).create(e)
  
  implicit def optionEnumValueTEF[A <: Enumeration#Value](ev: Enumeration#Value) = new TypedExpressionFactory[Option[A],TOptionEnumValue[A]] with DeOptionizer[A,TEnumValue[A],Option[A],TOptionEnumValue[A]] {
    val deOptionizer = enumValueTEF[A](ev)
  }
  
  object DummyEnum extends Enumeration {
    type DummyEnum = Value
    val DummyEnumerationValue = Value(-1, "DummyEnumerationValue")
  }
  
  implicit def optionEnumcValueToTE[A <: Enumeration#Value](e: Option[A]) = optionEnumValueTEF(e.getOrElse(DummyEnum.DummyEnumerationValue)).create(e)
  
  // =========================== Numerical Integral =========================== 

  implicit val byteTEF = new IntegralTypedExpressionFactory[Byte,TByte,Float,TFloat] with PrimitiveJdbcMapper[Byte] {
    val sample = 1: Byte
    val defaultColumnLength = 1
    val floatifyer = floatTEF
    def doMap(rs: ResultSet, i: Int) = rs.getByte(i)
  }
  
  implicit def byteToTE(f: Byte) = byteTEF.create(f)  

  implicit val optionByteTEF = new IntegralTypedExpressionFactory[Option[Byte],TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, TByte, Option[Byte], TOptionByte] {
    val deOptionizer = byteTEF
    val floatifyer = optionFloatTEF
  }
  
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)  
    
  implicit val intTEF = new IntegralTypedExpressionFactory[Int,TInt,Float,TFloat] with PrimitiveJdbcMapper[Int] {
    val sample = 1
    val defaultColumnLength = 4
    val floatifyer = floatTEF
    def doMap(rs: ResultSet, i: Int) = rs.getInt(i)
  }  
    
  implicit def intToTE(f: Int) = intTEF.create(f)  

  implicit val optionIntTEF = new IntegralTypedExpressionFactory[Option[Int],TOptionInt,Option[Float],TOptionFloat] with DeOptionizer[Int,TInt,Option[Int],TOptionInt] {
    val deOptionizer = intTEF
    val floatifyer = optionFloatTEF
  }

  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)
  
  implicit val longTEF = new IntegralTypedExpressionFactory[Long,TLong,Double,TDouble] with PrimitiveJdbcMapper[Long] {
    val sample = 1L
    val defaultColumnLength = 8
    val floatifyer = doubleTEF
    def doMap(rs: ResultSet, i: Int) = rs.getLong(i)
  }
  
  implicit def longToTE(f: Long) = longTEF.create(f)

  implicit val optionLongTEF = new IntegralTypedExpressionFactory[Option[Long],TOptionLong,Option[Double],TOptionDouble] with DeOptionizer[Long,TLong,Option[Long],TOptionLong] {
    val deOptionizer = longTEF
    val floatifyer = optionDoubleTEF
  }
  
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)  
  
  // =========================== Numerical Floating Point =========================== 
  
  implicit val floatTEF = new FloatTypedExpressionFactory[Float,TFloat] with PrimitiveJdbcMapper[Float] {
    val sample = 1F
    val defaultColumnLength = 4
    def doMap(rs: ResultSet, i: Int) = rs.getFloat(i)
  }
    
  implicit def floatToTE(f: Float) = floatTEF.create(f)
  
  implicit val optionFloatTEF = new FloatTypedExpressionFactory[Option[Float],TOptionFloat] with DeOptionizer[Float,TFloat,Option[Float],TOptionFloat] {
    val deOptionizer = floatTEF
  }

  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)
  
  implicit val doubleTEF = new FloatTypedExpressionFactory[Double,TDouble] with PrimitiveJdbcMapper[Double] {
    val sample = 1D
    val defaultColumnLength = 8
    def doMap(rs: ResultSet, i: Int) = rs.getDouble(i)
  }

  implicit def doubleToTE(f: Double) = doubleTEF.create(f)    
  
  implicit val optionDoubleTEF = new FloatTypedExpressionFactory[Option[Double],TOptionDouble] with DeOptionizer[Double,TDouble,Option[Double],TOptionDouble] {
    val deOptionizer = doubleTEF
  }
  
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)
  
  implicit val bigDecimalTEF = new FloatTypedExpressionFactory[BigDecimal,TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] {
    val sample = BigDecimal(1)
    val defaultColumnLength = -1
    def doMap(rs: ResultSet, i: Int) = BigDecimal(rs.getBigDecimal(i))
  }

  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)
  
  implicit val optionBigDecimalTEF = new FloatTypedExpressionFactory[Option[BigDecimal],TOptionBigDecimal] with DeOptionizer[BigDecimal,TBigDecimal,Option[BigDecimal],TOptionBigDecimal] {
    val deOptionizer = bigDecimalTEF
  }
    
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)
  
  
  def dump = println(registry.mkString("\n"))
  
  
  register(byteTEF)
  register(intTEF)
  register(longTEF)
  register(floatTEF)
  register(doubleTEF)  
  register(bigDecimalTEF)
  
  register(binaryTEF)
  register(booleanTEF)
  register(stringTEF)
  register(timestampTEF)
  register(dateTEF)  
  register(uuidTEF)
  val re = enumValueTEF(DummyEnum.DummyEnumerationValue)    
  
 /**
   * Enumerations are treated differently, since the map method should normally
   * return the actual Enumeration#value, but given that an enum is not only
   * determined by the int value from the DB, but also the parent Enumeration
   * parentEnumeration.values.find(_.id == v), the conversion is done 
   * in FieldMetaData.canonicalEnumerationValueFor(i: Int) 
   */
  val z = new FieldAttributesBasedOnType[Any](
      new {
        def map(rs:ResultSet,i:Int) = rs.getInt(i)
        def convertToJdbc(v: AnyRef) = v
      }, 
      re.defaultColumnLength, 
      re.sample,
      classOf[java.lang.Integer])
      
  registry.put(z.clasz, z)
  registry.put(z.clasz.getSuperclass, z)
  
    
  type MapperForReflection = {
    def map(rs:ResultSet,i:Int): Any
    def convertToJdbc(v: AnyRef): AnyRef
  }
   
  def makeMapper(fa0: JdbcMapper[_,_]) = new {
    val fa = fa0.asInstanceOf[JdbcMapper[AnyRef,AnyRef]]
    
    def map(rs:ResultSet,i:Int) = fa.map(rs, i)
    
    def convertToJdbc(v: AnyRef): AnyRef = {
      fa.convertToJdbc(v)
    }    
  }
  
  class FieldAttributesBasedOnType[A](val mapper: MapperForReflection, val defaultLength: Int, val sample: A, val nativeJdbcType: Class[_]) {

    val clasz: Class[_] = sample.getClass    

    override def toString = 
      clasz.getCanonicalName + " --> " + mapper.getClass.getCanonicalName    
  }
  
  def nativeJdbcValueFor(nonNativeType: Class[_], r: AnyRef) =   
    get(nonNativeType).mapper.convertToJdbc(r)
  
  def isSupported(c: Class[_]) =
    lookup(c) != None ||
    c.isAssignableFrom(classOf[Some[_]]) ||
    classOf[Product1[Any]].isAssignableFrom(c)
  
  def defaultColumnLength(c: Class[_]) =
    get(c).defaultLength

  def nativeJdbcTypeFor(c: Class[_]) =
    get(c).nativeJdbcType
    
  def resultSetHandlerFor(c: Class[_]): (ResultSet,Int) => AnyRef = {
    val fa = get(c) 
    (rs:ResultSet,i:Int) => {
       val z = fa.mapper.map(rs,i)
       if(rs.wasNull) null
       else z.asInstanceOf[AnyRef]
    }
  }
      
  
  private def get(c: Class[_]) =
    lookup(c).getOrElse(
      Utils.throwError("Usupported native type " + c.getCanonicalName + "," + c.getName + "\n" + registry.mkString("\n")))
  
  def sampleValueFor(c: Class[_]): AnyRef =
    get(c).sample.asInstanceOf[AnyRef]
    
  def register[P,A](m: NonPrimitiveJdbcMapper[P,A,_]) {
    
    val z = new FieldAttributesBasedOnType(
        makeMapper(m), 
        m.defaultColumnLength, 
        m.sample,
        m.primitiveMapper.nativeJdbcType)
    registry.put(z.clasz, z)
  }
  
  private def register[A](pm: PrimitiveJdbcMapper[A]) {
    val f = pm.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(
        makeMapper(pm), 
        f.defaultColumnLength, f.sample, pm.nativeJdbcType)
    
    val c = z.clasz
    
    registry.put(c, z)    
  }
    
  private def lookup(c: Class[_]): Option[FieldAttributesBasedOnType[_]] =
    if(!c.isPrimitive) 
      registry.get(c)
    else c.getName match {
      case "int" => lookup(classOf[java.lang.Integer])
      case "long" => lookup(classOf[java.lang.Long])
      case "float" => lookup(classOf[java.lang.Float])
      case "byte" => lookup(classOf[java.lang.Byte])
      case "boolean" => lookup(classOf[java.lang.Boolean])
      case "double" => lookup(classOf[java.lang.Double])
      case "void" => None
    }           
}
*/