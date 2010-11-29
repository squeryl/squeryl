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


import org.squeryl.internals.FieldReferenceLinker
import java.util.Date
import org.squeryl.dsl.ast.{SelectElement, SelectElementReference, ConstantExpressionNode}
import org.squeryl.dsl._
import java.sql.Timestamp

trait CustomType extends Product1[Any] {
  def canEqual(a:Any) = false
}

trait CustomTypesMode extends QueryDsl {

  implicit def createConstantNodeOfScalarIntType(i: Int) =
    new ConstantExpressionNode[Int](i) with NumericalExpression[Int]

  implicit def createConstantNodeOfScalarStringType(s: String) =
    new ConstantExpressionNode[String](s) with StringExpression[String]

  implicit def createConstantNodeOfScalarDoubleType(i: Double) =
    new ConstantExpressionNode[Double](i) with NumericalExpression[Double]

  implicit def createConstantNodeOfScalarBigDecimalType(i: BigDecimal) =
    new ConstantExpressionNode[BigDecimal](i) with NumericalExpression[BigDecimal]

  implicit def createConstantNodeOfScalarFloatType(i: Float) =
    new ConstantExpressionNode[Float](i) with NumericalExpression[Float]

  implicit def createConstantNodeOfScalarLongType(i: Long) =
    new ConstantExpressionNode[Long](i) with NumericalExpression[Long]

  implicit def createConstantNodeOfScalarBooleanType(i: Boolean) =
    new ConstantExpressionNode[Boolean](i) with NonNumericalExpression[Boolean]

  implicit def createConstantNodeOfScalarBinaryType(i: Array[Byte]) =
    new ConstantExpressionNode[Array[Byte]](i) with BinaryExpression[Array[Byte]]

  type ByteType = ByteField

  type IntType = IntField

  type StringType = StringField

  type DoubleType = DoubleField

  type BigDecimalType = BigDecimalField

  type FloatType = FloatField

  type LongType = LongField

  type BooleanType = BooleanField

  type DateType = DateField

  type TimestampType = TimestampField

  type EnumerationValueType = Enumeration#Value

  type BinaryType = BinaryField
  
  protected def mapByte2ByteType(i: Byte) = new ByteField(i)
  protected def mapInt2IntType(i: Int) = new IntField(i)
  protected def mapString2StringType(s: String) = new StringField(s)
  protected def mapDouble2DoubleType(d: Double) = new DoubleField(d)
  protected def mapBigDecimal2BigDecimalType(d: BigDecimal) = new BigDecimalField(d)
  protected def mapFloat2FloatType(d: Float) = new FloatField(d)
  protected def mapLong2LongType(l: Long) = new LongField(l)
  protected def mapBoolean2BooleanType(b: Boolean) = new BooleanField(b)
  protected def mapDate2DateType(b: Date) = new DateField(b)
  protected def mapTimestamp2TimestampType(b: Timestamp) = new TimestampField(b)
  //protected def mapInt2EnumerationValueType(b: Int): EnumerationValueType
  protected def mapBinary2BinaryType(d: Array[Byte]) = new BinaryField(d)

  protected implicit val sampleByte: ByteType = new ByteField(0)
  protected implicit val sampleInt = new IntField(0)
  protected implicit val sampleString: StringType = new StringField("")
  protected implicit val sampleDouble: DoubleType = new DoubleField(0.0)
  protected implicit val sampleBigDecimal: BigDecimalType = new BigDecimalField(BigDecimal(0))
  protected implicit val sampleFloat: FloatType = new FloatField(0.0F)
  protected implicit val sampleLong = new LongField(1)
  protected implicit val sampleBoolean = new BooleanField(false)
  protected implicit val sampleDate = new DateField(new Date)
  protected implicit def sampleTimestamp = new TimestampField(new Timestamp(0))
  protected implicit val sampleBinary: BinaryType = new BinaryField(Array[Byte](0))

  
  //TODO Scala bug report, implicit params should work here , but they don't ...
  def createLeafNodeOfScalarIntType(i: IntField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[IntType](i) with NumericalExpression[IntType]
      case Some(n:SelectElement) =>
        new SelectElementReference[IntType](n)(createOutMapperIntType) with NumericalExpression[IntType]
    }

  def createLeafNodeOfScalarIntOptionType(i: Option[IntField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[IntType]](i) with NumericalExpression[Option[IntType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[IntType]](n)(createOutMapperIntTypeOption) with NumericalExpression[Option[IntType]]
    }

  def createLeafNodeOfScalarStringType(s: StringField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[StringType](s) with StringExpression[StringType]
      case Some(n:SelectElement) =>
        new SelectElementReference[StringType](n)(createOutMapperStringType) with StringExpression[StringType]
    }

  def createLeafNodeOfScalarStringOptionType(s: Option[StringField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[StringType]](s) with StringExpression[Option[StringType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[StringType]](n)(createOutMapperStringTypeOption) with StringExpression[Option[StringType]]
    }

  def createLeafNodeOfScalarDoubleType(i: DoubleField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[DoubleType](i) with NumericalExpression[DoubleType]
      case Some(n:SelectElement) =>
        new SelectElementReference[DoubleType](n)(createOutMapperDoubleType) with NumericalExpression[DoubleType]
    }

  def createLeafNodeOfScalarDoubleOptionType(i: Option[DoubleField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[DoubleType]](i) with NumericalExpression[Option[DoubleType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[DoubleType]](n)(createOutMapperDoubleTypeOption) with  NumericalExpression[Option[DoubleType]]
    }

  def createLeafNodeOfScalarBigDecimalType(i: BigDecimalField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[BigDecimalType](i) with NumericalExpression[BigDecimalType]
      case Some(n:SelectElement) =>
        new SelectElementReference[BigDecimalType](n)(createOutMapperBigDecimalType) with  NumericalExpression[BigDecimalType]
    }

  def createLeafNodeOfScalarBigDecimalOptionType(i: Option[BigDecimalField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[BigDecimalType]](i) with NumericalExpression[Option[BigDecimalType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[BigDecimalType]](n)(createOutMapperBigDecimalTypeOption) with  NumericalExpression[Option[BigDecimalType]]
    }

  def createLeafNodeOfScalarFloatType(i: FloatField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[FloatType](i) with NumericalExpression[FloatType]
      case Some(n:SelectElement) =>
        new SelectElementReference[FloatType](n)(createOutMapperFloatType) with NumericalExpression[FloatType]
    }

  def createLeafNodeOfScalarFloatOptionType(i: Option[FloatField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[FloatType]](i) with NumericalExpression[Option[FloatType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[FloatType]](n)(createOutMapperFloatTypeOption) with  NumericalExpression[Option[FloatType]]
    }

  def createLeafNodeOfScalarLongType(i: LongField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[LongType](i) with NumericalExpression[LongType]
      case Some(n:SelectElement) =>
        new SelectElementReference[LongType](n)(createOutMapperLongType) with  NumericalExpression[LongType]
    }

  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[LongType]](l) with NumericalExpression[Option[LongType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[LongType]](n)(createOutMapperLongTypeOption) with  NumericalExpression[Option[LongType]]
    }

  def createLeafNodeOfScalarBooleanType(i: BooleanField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[BooleanType](i) with BooleanExpression[BooleanType]
      case Some(n:SelectElement) =>
        new SelectElementReference[BooleanType](n)(createOutMapperBooleanType) with  BooleanExpression[BooleanType]
    }

  def createLeafNodeOfScalarBooleanOptionType(i: Option[BooleanField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[BooleanType]](i) with BooleanExpression[Option[BooleanType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[BooleanType]](n)(createOutMapperBooleanTypeOption) with  BooleanExpression[Option[BooleanType]]
    }

  def createLeafNodeOfScalarDateType(i: DateField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[DateType](i) with DateExpression[DateType]
      case Some(n:SelectElement) =>
        new SelectElementReference[DateType](n)(createOutMapperDateType) with  DateExpression[DateType]
    }

  def createLeafNodeOfScalarDateOptionType(i: Option[DateField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[DateType]](i) with DateExpression[Option[DateType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[DateType]](n)(createOutMapperDateTypeOption) with  DateExpression[Option[DateType]]
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

  def createLeafNodeOfScalarTimestampType(d: TimestampField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[TimestampType](d) with DateExpression[TimestampType]
      case Some(n:SelectElement) =>
        new SelectElementReference[TimestampType](n) with DateExpression[TimestampType]
    }

  def createLeafNodeOfScalarTimestampOptionType(d: Option[TimestampField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[TimestampType]](d) with DateExpression[Option[TimestampType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[TimestampType]](n) with DateExpression[Option[TimestampType]]
    }
  
  def createLeafNodeOfScalarBinaryType(i: BinaryField) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[BinaryType](i) with BinaryExpression[BinaryType]
      case Some(n:SelectElement) =>
        new SelectElementReference[BinaryType](n)(createOutMapperBinaryType) with BinaryExpression[BinaryType]
    }

  def createLeafNodeOfScalarBinaryOptionType(i: Option[BinaryField]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[BinaryType]](i) with BinaryExpression[Option[BinaryType]]
      case Some(n:SelectElement) =>
        new SelectElementReference[Option[BinaryType]](n)(createOutMapperBinaryTypeOption) with BinaryExpression[Option[BinaryType]]
    }

}

object CustomTypesMode extends CustomTypesMode 


class ByteField(val value: Byte) extends CustomType {
  def _1: Any = value
}

class IntField(val value: Int) extends CustomType {
  def _1: Any = value
}

class StringField(val value: String) extends CustomType {
  def _1: Any = value
}

class DoubleField(val value: Double) extends CustomType {
  def _1: Any = value
}

class BigDecimalField(val value: BigDecimal) extends CustomType {
  def _1: Any = value
}

class FloatField(val value: Float) extends CustomType {
  def _1: Any = value
}

class LongField(val value: Long) extends CustomType {
  def _1: Any = value
}

class BooleanField(val value: Boolean) extends CustomType {
  def _1: Any = value
}

class DateField(val value: Date) extends CustomType {
  def _1: Any = value
}

class TimestampField(val value: Timestamp) extends CustomType {
  def _1: Any = value
}

class BinaryField(val value: Array[Byte]) extends CustomType {
  def _1: Any = value
}
