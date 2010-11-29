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
import java.util.Date
import java.sql.Timestamp

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

  type ByteType = Byte

  type IntType = Int

  type StringType = String

  type DoubleType = Double

  type BigDecimalType = BigDecimal

  type FloatType = Float

  type LongType = Long

  type BooleanType = Boolean

  type DateType = Date

  type TimestampType = Timestamp

  type EnumerationValueType = Enumeration#Value

  type BinaryType = Array[Byte]

  //TODO: consider spliting createLeafNodeOfScalarIntType in two factory methods : createConstantOfXXXType and createReferenceOfXXXType 
  
  def createLeafNodeOfScalarIntType(i: IntType) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Int](i) with NumericalExpression[IntType]
      case Some(n:SelectElement) =>
        new SelectElementReference[IntType](n) with NumericalExpression[IntType]
    }

  def createLeafNodeOfScalarIntOptionType(i: Option[IntType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Int]](i) with NumericalExpression[Option[IntType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Int]](n) with NumericalExpression[Option[Int]]
    }

  def createLeafNodeOfScalarStringType(s: String) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[String](s) with StringExpression[String]
      case Some(n:SelectElement) =>
        new SelectElementReference[String](n) with StringExpression[String]
    }
  }

  def createLeafNodeOfScalarStringOptionType(s: Option[StringType]) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[String]](s) with StringExpression[Option[String]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[String]](n) with StringExpression[Option[String]]
    }
  }

  def createLeafNodeOfScalarDoubleType(i: Double) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Double](i) with NumericalExpression[Double]
      case Some(n:SelectElement) =>
        new SelectElementReference[Double](n) with  NumericalExpression[Double]
    }

  def createLeafNodeOfScalarDoubleOptionType(i: Option[Double]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Double]](i) with NumericalExpression[Option[Double]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Double]](n) with NumericalExpression[Option[Double]]
    }


  def createLeafNodeOfScalarBigDecimalType(i: BigDecimal) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[BigDecimal](i) with NumericalExpression[BigDecimal]
      case Some(n:SelectElement) =>
        new SelectElementReference[BigDecimal](n) with  NumericalExpression[BigDecimal]
    }

  def createLeafNodeOfScalarBigDecimalOptionType(i: Option[BigDecimal]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[BigDecimal]](i) with NumericalExpression[Option[BigDecimal]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[BigDecimal]](n) with NumericalExpression[Option[BigDecimal]]
    }


  def createLeafNodeOfScalarFloatType(i: Float) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Float](i) with NumericalExpression[Float]
      case Some(n:SelectElement) =>
        new SelectElementReference[Float](n) with  NumericalExpression[Float]
    }

  def createLeafNodeOfScalarFloatOptionType(i: Option[Float]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Float]](i) with NumericalExpression[Option[Float]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Float]](n) with  NumericalExpression[Option[Float]]
    }

  def createLeafNodeOfScalarLongType(i: Long) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Long](i) with NumericalExpression[Long]
      case Some(n:SelectElement) =>
        new SelectElementReference[Long](n) with  NumericalExpression[Long]
    }

  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Long]](l) with NumericalExpression[Option[Long]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Long]](n) with NumericalExpression[Option[Long]]
    }

  def createLeafNodeOfScalarBooleanType(i: Boolean) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Boolean](i) with BooleanExpression[Boolean]
      case Some(n:SelectElement) =>
        new SelectElementReference[Boolean](n) with  BooleanExpression[Boolean]
    }

  def createLeafNodeOfScalarBooleanOptionType(b: Option[BooleanType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Boolean]](b) with BooleanExpression[Option[Boolean]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Boolean]](n) with BooleanExpression[Option[Boolean]]
    }

  def createLeafNodeOfScalarBinaryType(i: BinaryType) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[BinaryType](i) with BinaryExpression[BinaryType]
      case Some(n:SelectElement) =>
        new SelectElementReference[BinaryType](n) with BinaryExpression[BinaryType]
    }

  def createLeafNodeOfScalarBinaryOptionType(i: Option[BinaryType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[BinaryType]](i) with BinaryExpression[Option[BinaryType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[BinaryType]](n) with BinaryExpression[Option[BinaryType]]
    }

  def createLeafNodeOfScalarDateType(i: Date) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Date](new java.sql.Date(i.getTime)) with DateExpression[Date]
      case Some(n:SelectElement) =>
        new SelectElementReference[Date](n) with DateExpression[Date]
    }

  def createLeafNodeOfScalarDateOptionType(b: Option[DateType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Date]](b) with DateExpression[Option[Date]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Date]](n) with  DateExpression[Option[Date]]
    }

  def createLeafNodeOfScalarTimestampType(d: Timestamp) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Timestamp](d) with DateExpression[Timestamp]
      case Some(n:SelectElement) =>
        new SelectElementReference[Timestamp](n) with DateExpression[Timestamp]
    }

  def createLeafNodeOfScalarTimestampOptionType(d: Option[Timestamp]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Timestamp]](d) with DateExpression[Option[Timestamp]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Timestamp]](n) with DateExpression[Option[Timestamp]]
    }

  def createLeafNodeOfEnumExpressionType[A](e: EnumerationValueType): EnumExpression[Enumeration#Value] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Enumeration#Value](e) with EnumExpression[Enumeration#Value]
      case Some(n:SelectElement) =>
        new SelectElementReference[Enumeration#Value](n)(n.createEnumerationMapper) with  EnumExpression[Enumeration#Value]
    }

  def createLeafNodeOfEnumExpressionOptionType[A](e: Option[EnumerationValueType]): EnumExpression[Option[Enumeration#Value]] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Enumeration#Value]](e) with EnumExpression[Option[Enumeration#Value]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[Enumeration#Value]](n)(n.createEnumerationOptionMapper) with  EnumExpression[Option[Enumeration#Value]]
    }

  protected def mapByte2ByteType(i: Byte) = i
  protected def mapInt2IntType(i: Int) = i
  protected def mapString2StringType(s: String) = s
  protected def mapDouble2DoubleType(d: Double) = d
  protected def mapBigDecimal2BigDecimalType(d: BigDecimal) = d
  protected def mapFloat2FloatType(d: Float) = d
  protected def mapLong2LongType(l: Long) = l
  protected def mapBoolean2BooleanType(b: Boolean) = b
  protected def mapDate2DateType(b: Date) = b
  protected def mapTimestamp2TimestampType(b: Timestamp) = b
  //protected def mapInt2EnumerationValueType(b: Int): EnumerationValueType
  protected def mapBinary2BinaryType(d: Array[Byte]) = d

  protected implicit val sampleByte: ByteType = 0xF.byteValue
  protected implicit val sampleInt: IntType = 0
  protected implicit val sampleString: StringType = ""
  protected implicit val sampleDouble: DoubleType = 0.0
  protected implicit val sampleBigDecimal: BigDecimalType = 0.0
  protected implicit val sampleFloat: FloatType = 0.0F
  protected implicit val sampleLong: LongType = 0
  protected implicit val sampleBoolean: BooleanType = false
  protected implicit val sampleDate: DateType = new Date
  protected implicit def sampleTimestamp: TimestampType = new Timestamp(0)
  //protected implicit def sampleEnumerationValueType: EnumerationValueType = DummyEnum.DummyEnumerationValue
  protected implicit val sampleBinary: BinaryType = Array[Byte](0)
}

object DummyEnum extends Enumeration {
  type DummyEnum = Value
  val DummyEnumerationValue = Value(-1, "DummyEnumerationValue")
}
