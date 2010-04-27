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
 ******************************************************************************/
package org.squeryl.customtypes;


import org.squeryl.dsl.QueryDsl
import org.squeryl.internals.FieldReferenceLinker
import org.squeryl.dsl.ast.{SelectElementReference, ConstantExpressionNode}
import java.util.Date


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

  implicit def createConstantNodeOfScalarFloatType(i: Float) =
    new ConstantExpressionNode[Float](i) with NumericalExpression[Float]

  implicit def createConstantNodeOfScalarLongType(i: Long) =
    new ConstantExpressionNode[Long](i) with NumericalExpression[Long]

  implicit def createConstantNodeOfScalarBooleanType(i: Boolean) =
    new ConstantExpressionNode[Boolean](i) with NonNumericalExpression[Boolean]

  type ByteType = ByteField

  type IntType = IntField

  type StringType = StringField

  type DoubleType = DoubleField

  type FloatType = FloatField

  type LongType = LongField

  type BooleanType = BooleanField

  type DateType = DateField

  protected def mapByte2ByteType(i: Byte) = new ByteField(i)
  protected def mapInt2IntType(i: Int) = new IntField(i)
  protected def mapString2StringType(s: String) = new StringField(s)
  protected def mapDouble2DoubleType(d: Double) = new DoubleField(d)
  protected def mapFloat2FloatType(d: Float) = new FloatField(d)
  protected def mapLong2LongType(l: Long) = new LongField(l)
  protected def mapBoolean2BooleanType(b: Boolean) = new BooleanField(b)
  protected def mapDate2DateType(b: Date) = new DateField(b)

  protected implicit val sampleByte: ByteType = new ByteField(0)
  protected implicit val sampleInt = new IntField(0)
  protected implicit val sampleString: StringType = new StringField("")
  protected implicit val sampleDouble: DoubleType = new DoubleField(0.0)
  protected implicit val sampleFloat: FloatType = new FloatField(0.0F)
  protected implicit val sampleLong = new LongField(1)
  protected implicit val sampleBoolean = new BooleanField(false)
  protected implicit val sampleDate = new DateField(new Date)
  //TODO Scala bug report, implicit params should work here , but they don't ...
  def createLeafNodeOfScalarIntType(i: IntField) =
    new SelectElementReference[IntType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperIntType) with NumericalExpression[IntType]
  def createLeafNodeOfScalarIntOptionType(i: Option[IntField]) =
    new SelectElementReference[Option[IntType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperIntTypeOption) with NumericalExpression[Option[IntType]]

  def createLeafNodeOfScalarStringType(s: StringField) =
    new SelectElementReference[StringType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperStringType) with StringExpression[StringType]
  def createLeafNodeOfScalarStringOptionType(s: Option[StringField]) =
    new SelectElementReference[Option[StringType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperStringTypeOption) with StringExpression[Option[StringType]]

  def createLeafNodeOfScalarDoubleType(i: DoubleField) =
    new SelectElementReference[DoubleType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDoubleType) with  NumericalExpression[DoubleType]
  def createLeafNodeOfScalarDoubleOptionType(i: Option[DoubleField]) =
    new SelectElementReference[Option[DoubleType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDoubleTypeOption) with  NumericalExpression[Option[DoubleType]]

  def createLeafNodeOfScalarFloatType(i: FloatField) =
    new SelectElementReference[FloatType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperFloatType) with  NumericalExpression[FloatType]
  def createLeafNodeOfScalarFloatOptionType(i: Option[FloatField]) =
    new SelectElementReference[Option[FloatType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperFloatTypeOption) with  NumericalExpression[Option[FloatType]]

  def createLeafNodeOfScalarLongType(i: LongField) =
    new SelectElementReference[LongType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperLongType) with  NumericalExpression[LongType]
  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    new SelectElementReference[Option[LongType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperLongTypeOption) with  NumericalExpression[Option[LongType]]

  def createLeafNodeOfScalarBooleanType(i: BooleanField) =
    new SelectElementReference[BooleanType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBooleanType) with  BooleanExpression[BooleanType]

  def createLeafNodeOfScalarBooleanOptionType(i: Option[BooleanField]) =
    new SelectElementReference[Option[BooleanType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBooleanTypeOption) with  BooleanExpression[Option[BooleanType]]

  def createLeafNodeOfScalarDateType(i: DateField) =
    new SelectElementReference[DateType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDateType) with  DateExpression[DateType]

  def createLeafNodeOfScalarDateOptionType(i: Option[DateField]) =
    new SelectElementReference[Option[DateType]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDateTypeOption) with  DateExpression[Option[DateType]]
  
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
