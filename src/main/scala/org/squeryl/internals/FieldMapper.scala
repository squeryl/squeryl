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
import java.sql.ResultSet
import java.sql.Timestamp
import java.util.Date
import java.util.UUID
import org.squeryl.dsl._
import java.util.regex.Pattern
import org.squeryl.Session
import org.squeryl.dsl.ArrayJdbcMapper

trait FieldMapper {
  outer =>
    
  private val registry = new HashMap[Class[_],FieldAttributesBasedOnType[_]]

  implicit def thisFieldMapper = this

  /**
   * Extending classes will expose members of PrimitiveTypeSupport as implicit, to enable
   * support of primitive types, or will expose theit own non jdbc native types. 
   */  
  
  protected object PrimitiveTypeSupport { 
    // =========================== Non Numerical =========================== 
    
    val stringTEF = new TypedExpressionFactory[String,TString] with PrimitiveJdbcMapper[String] {
      val sample = "": String
      val defaultColumnLength = 128
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getString(i)
    }
        
    val optionStringTEF = new TypedExpressionFactory[Option[String],TOptionString] with DeOptionizer[String, String, TString, Option[String], TOptionString]{
      val deOptionizer = stringTEF
    }
    
    val dateTEF = new TypedExpressionFactory[Date,TDate] with PrimitiveJdbcMapper[Date] {
      val sample = new Date
      val defaultColumnLength = -1
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getDate(i)
    }

    val sqlDateTEF = new TypedExpressionFactory[java.sql.Date,TDate] with PrimitiveJdbcMapper[java.sql.Date] {
      val sample = new java.sql.Date(0L)
      val defaultColumnLength = -1
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getDate(i)
    }
    
    val optionDateTEF = new TypedExpressionFactory[Option[Date],TOptionDate] with DeOptionizer[Date, Date, TDate, Option[Date], TOptionDate] {
      val deOptionizer = dateTEF
    }

    val optionSqlDateTEF = new TypedExpressionFactory[Option[java.sql.Date],TOptionDate] with DeOptionizer[java.sql.Date, java.sql.Date, TDate, Option[java.sql.Date], TOptionDate] {
      val deOptionizer = sqlDateTEF
    }

    val timestampTEF = new TypedExpressionFactory[Timestamp,TTimestamp] with PrimitiveJdbcMapper[Timestamp] {
      val sample = new Timestamp(0)
      val defaultColumnLength = -1
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getTimestamp(i)
    }
  
    val optionTimestampTEF = new TypedExpressionFactory[Option[Timestamp],TOptionTimestamp] with DeOptionizer[Timestamp, Timestamp, TTimestamp, Option[Timestamp], TOptionTimestamp] {
      val deOptionizer = timestampTEF
    }
    
    val booleanTEF = new TypedExpressionFactory[Boolean,TBoolean] with PrimitiveJdbcMapper[Boolean] {
      val sample = true
      val defaultColumnLength = 1
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getBoolean(i)
    }
    
    val optionBooleanTEF = new TypedExpressionFactory[Option[Boolean],TOptionBoolean] with DeOptionizer[Boolean, Boolean, TBoolean, Option[Boolean], TOptionBoolean] {
      val deOptionizer = booleanTEF
    }
  
    val uuidTEF = new TypedExpressionFactory[UUID,TUUID] with PrimitiveJdbcMapper[UUID] {
      val sample = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
      val defaultColumnLength = 36 
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = {
        val v = rs.getObject(i)
        v match {
            case u: UUID => u
            case s: String => UUID.fromString(s)
            case _ => sample
        }
      }
    }
        
    val optionUUIDTEF = new TypedExpressionFactory[Option[UUID],TOptionUUID] with DeOptionizer[UUID, UUID, TUUID, Option[UUID], TOptionUUID] {
      val deOptionizer = uuidTEF
    }
    
    val binaryTEF = new TypedExpressionFactory[Array[Byte],TByteArray] with PrimitiveJdbcMapper[Array[Byte]] {
      val sample = Array(0: Byte)   
      val defaultColumnLength = 255
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getBytes(i)
    }
    
    val optionByteArrayTEF = new TypedExpressionFactory[Option[Array[Byte]],TOptionByteArray] with DeOptionizer[Array[Byte], Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] {
      val deOptionizer = binaryTEF
    }
    
    val intArrayTEF = new ArrayTEF[Int, TIntArray] {
      val sample = Array(0)
      def toWrappedJDBCType(element: Int) : java.lang.Object = new java.lang.Integer(element)
      def fromWrappedJDBCType(elements: Array[java.lang.Object]) : Array[Int] = elements.map(i => i.asInstanceOf[java.lang.Integer].toInt)
    }
    
    val longArrayTEF = new ArrayTEF[Long, TLongArray] {
      val sample = Array(0L)
      def toWrappedJDBCType(element: Long) : java.lang.Object = new java.lang.Long(element)
      def fromWrappedJDBCType(elements: Array[java.lang.Object]) : Array[Long] = elements.map(i => i.asInstanceOf[java.lang.Long].toLong)
    }
    
    val doubleArrayTEF = new ArrayTEF[Double, TDoubleArray] {
      val sample : Array[Double] = Array(0.0)
      def toWrappedJDBCType(element: Double) : java.lang.Object = new java.lang.Double(element)
      def fromWrappedJDBCType(elements: Array[java.lang.Object]) : Array[Double] = elements.map(i => i.asInstanceOf[java.lang.Double].toDouble)
    }

    val stringArrayTEF = new ArrayTEF[String, TStringArray] {
      val sample : Array[String] = Array("")
      def toWrappedJDBCType(element: String) : java.lang.Object = new java.lang.String(element)
      def fromWrappedJDBCType(elements: Array[java.lang.Object]) : Array[String] = elements.map(i => i.asInstanceOf[java.lang.String].toString)
    }
    
    // FIXME: The type soup on this was beyond my patience for now...I think we'll need an ArrayDeOptionizer
    //val optionIntArrayTEF = new TypedExpressionFactory[Option[Array[Int]],TOptionIntArray] with DeOptionizer[Array[Int], Array[Int], TIntArray, Option[Array[Int]], TOptionIntArray] {
      //val deOptionizer = intArrayTEF
    //}
    
    def enumValueTEF[A >: Enumeration#Value <: Enumeration#Value](ev: Enumeration#Value) = 
      new JdbcMapper[Int,A] with TypedExpressionFactory[A,TEnumValue[A]] { 
        
      val enu = Utils.enumerationForValue(ev)
      
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getInt(i)
      def defaultColumnLength: Int = intTEF.defaultColumnLength
      def sample: A = ev
      def convertToJdbc(v: A) = v.id
      def convertFromJdbc(v: Int) = {
        enu.values.find(_.id == v).getOrElse(DummyEnum.DummyEnumerationValue) // JDBC has no concept of null value for primitive types (ex. Int)
        // at this level, we mimic this JDBC flaw (the Option / None based on jdbc.wasNull will get sorted out by optionEnumValueTEF)
      }
    }    
    
    object DummyEnum extends Enumeration {
      type DummyEnum = Value
      val DummyEnumerationValue = Value(-1, "DummyEnumerationValue")
    }
    
    def optionEnumValueTEF[A >: Enumeration#Value <: Enumeration#Value](ev: Option[Enumeration#Value]) = new TypedExpressionFactory[Option[A],TOptionEnumValue[A]] with DeOptionizer[Int,A,TEnumValue[A],Option[A],TOptionEnumValue[A]] {
      val deOptionizer = {
        val e = ev.getOrElse(PrimitiveTypeSupport.DummyEnum.DummyEnumerationValue)
        enumValueTEF[A](e)
      }
    }

    // =========================== Numerical Integral =========================== 
  
    val byteTEF = new IntegralTypedExpressionFactory[Byte,TByte,Float,TFloat] with PrimitiveJdbcMapper[Byte] {
      val sample = 1: Byte
      val defaultColumnLength = 1
      val floatifyer = floatTEF
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getByte(i)
    }
    
    val optionByteTEF = new IntegralTypedExpressionFactory[Option[Byte],TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] {
      val deOptionizer = byteTEF
      val floatifyer = optionFloatTEF
    }
        
    val intTEF = new IntegralTypedExpressionFactory[Int,TInt,Float,TFloat] with PrimitiveJdbcMapper[Int] {
      val sample = 1
      val defaultColumnLength = 4
      val floatifyer = floatTEF
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getInt(i)
    }  
    
    val optionIntTEF = new IntegralTypedExpressionFactory[Option[Int],TOptionInt,Option[Float],TOptionFloat] with DeOptionizer[Int,Int,TInt,Option[Int],TOptionInt] {
      val deOptionizer = intTEF
      val floatifyer = optionFloatTEF
    }
    
    val longTEF = new IntegralTypedExpressionFactory[Long,TLong,Double,TDouble] with PrimitiveJdbcMapper[Long] {
      val sample = 1L
      val defaultColumnLength = 8
      val floatifyer = doubleTEF
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getLong(i)
    }
  
    val optionLongTEF = new IntegralTypedExpressionFactory[Option[Long],TOptionLong,Option[Double],TOptionDouble] with DeOptionizer[Long,Long,TLong,Option[Long],TOptionLong] {
      val deOptionizer = longTEF
      val floatifyer = optionDoubleTEF
    }
    
    // =========================== Numerical Floating Point =========================== 
    
    val floatTEF = new FloatTypedExpressionFactory[Float,TFloat] with PrimitiveJdbcMapper[Float] {
      val sample = 1F
      val defaultColumnLength = 4
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getFloat(i)
    }
    
    val optionFloatTEF = new FloatTypedExpressionFactory[Option[Float],TOptionFloat] with DeOptionizer[Float,Float,TFloat,Option[Float],TOptionFloat] {
      val deOptionizer = floatTEF
    }
    
    val doubleTEF = new FloatTypedExpressionFactory[Double,TDouble] with PrimitiveJdbcMapper[Double] {
      val sample = 1D
      val defaultColumnLength = 8
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getDouble(i)
    }
    
    val optionDoubleTEF = new FloatTypedExpressionFactory[Option[Double],TOptionDouble] with DeOptionizer[Double,Double,TDouble,Option[Double],TOptionDouble] {
      val deOptionizer = doubleTEF
    }
    
    val bigDecimalTEF = new FloatTypedExpressionFactory[BigDecimal,TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] {
      val sample = BigDecimal(1)
      val defaultColumnLength = -1
      def extractNativeJdbcValue(rs: ResultSet, i: Int) = BigDecimal(rs.getBigDecimal(i))
    }
    
    val optionBigDecimalTEF = new FloatTypedExpressionFactory[Option[BigDecimal],TOptionBigDecimal] with DeOptionizer[BigDecimal,BigDecimal,TBigDecimal,Option[BigDecimal],TOptionBigDecimal] {
      val deOptionizer = bigDecimalTEF
    }  
  }
  
  initialize
  
  protected def initialize {
    import PrimitiveTypeSupport._
    
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
    register(sqlDateTEF)
    register(uuidTEF)
    register(intArrayTEF)
    register(longArrayTEF)
    register(doubleArrayTEF)
    register(stringArrayTEF)

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
  }  
    
  protected type MapperForReflection = {
    def map(rs:ResultSet,i:Int): Any
    def convertToJdbc(v: AnyRef): AnyRef
  }
   
  protected def makeMapper(fa0: JdbcMapper[_,_]) = new {
    val fa = fa0.asInstanceOf[JdbcMapper[AnyRef,AnyRef]]
    
    def map(rs:ResultSet,i:Int) = fa.map(rs, i)
    
    def convertToJdbc(v: AnyRef): AnyRef = {
      if(v != null)
        fa.convertToJdbc(v)
      else null
    }    
  }
  
  protected class FieldAttributesBasedOnType[A](val mapper: MapperForReflection, val defaultLength: Int, val sample: A, val nativeJdbcType: Class[_]) {

    val clasz: Class[_] = sample.asInstanceOf[AnyRef].getClass

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
    lookup(c).
      getOrElse(
        Utils.throwError("Usupported native type " + c.getCanonicalName + "," + c.getName + "\n" + registry.mkString("\n")))  
  
  def sampleValueFor(c: Class[_]): AnyRef =
    get(c).sample.asInstanceOf[AnyRef]
    
  def trySampleValueFor(c: Class[_]): AnyRef = {
    val r = lookup(c).map(_.sample)    
    r match {
      case Some(x:AnyRef) => x
      case _ => null
    }
  }
  
  private [squeryl] def register[P,A](m: NonPrimitiveJdbcMapper[P,A,_]) {
    
    val z = new FieldAttributesBasedOnType(
        makeMapper(m), 
        m.defaultColumnLength, 
        m.sample,
        m.primitiveMapper.nativeJdbcType)
    
    val wasThere = registry.put(z.clasz, z)
    
    if(wasThere != None)
      Utils.throwError("field type "+ z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }
  
  private [squeryl] def register[S,J](m: ArrayJdbcMapper[S,J]) {
    val f = m.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(
        makeMapper(m), 
        m.defaultColumnLength, 
        f.sample,
        m.nativeJdbcType)
    
    val wasThere = registry.put(z.clasz, z)
    
    if(wasThere != None)
      Utils.throwError("field type "+ z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }
  
  private def register[A](pm: PrimitiveJdbcMapper[A]) {
    val f = pm.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(
        makeMapper(pm), 
        f.defaultColumnLength, f.sample, pm.nativeJdbcType)
    
    val c = z.clasz
    
    registry.put(c, z)    
  }
  
  private def lookup(c: Class[_]): Option[FieldAttributesBasedOnType[_]] = {
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
}
