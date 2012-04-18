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
package org.squeryl.dsl

import ast._
import org.squeryl.{Schema, Session, Query}
import org.squeryl.internals.{AttributeValidOnNonNumericalColumn, AttributeValidOnNumericalColumn, StatementWriter}

trait FieldTypes {
  self: TypeArithmetic =>
  

  type ByteType

  type IntType

  type StringType

  type FloatType

  type DoubleType

  type LongType

  type BooleanType

  type DateType

  type TimestampType

  type BigDecimalType

  type EnumerationValueType

  type UuidType

  type BinaryType

  protected implicit def sampleByte: ByteType
  protected implicit def sampleInt: IntType
  protected implicit def sampleString: StringType
  protected implicit def sampleDouble: DoubleType
  protected implicit def sampleFloat: FloatType
  protected implicit def sampleLong: LongType
  protected implicit def sampleBoolean: BooleanType
  protected implicit def sampleDate: DateType
  protected implicit def sampleTimestamp: TimestampType
  protected implicit def sampleBigDecimal: BigDecimalType
  //protected implicit def sampleEnumerationValueType: EnumerationValueType
  protected implicit def sampleBinary: BinaryType
  protected implicit def sampleUuid: UuidType

  protected implicit val sampleByteO = Some(sampleByte)
  protected implicit val sampleIntO = Some(sampleInt)
  protected implicit val sampleStringO = Some(sampleString)
  protected implicit val sampleDoubleO = Some(sampleDouble)
  protected implicit val sampleFloatO = Some(sampleFloat)
  protected implicit val sampleLongO = Some(sampleLong)
  protected implicit val sampleBooleanO = Some(sampleBoolean)
  protected implicit val sampleDateO = Some(sampleDate)
  protected implicit val sampleTimestampTypeO = Some(sampleTimestamp)
  protected implicit val sampleBigDecimalO = Some(sampleBigDecimal)
  //protected implicit val sampleEnumerationValueTypeO = Some(sampleEnumerationValueType)
  protected implicit val sampleBinaryO = Some(sampleBinary)
  protected implicit val sampleUuidO = Some(sampleUuid)
}


trait NumericalExpression[A] extends TypedExpressionNode[A] {

  def ===[B](b: NumericalExpression[B]) = new EqualityExpression(this, b)
  def <>[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<>")

  def > [B](b: NumericalExpression[B]) = gt(b)
  def >=[B](b: NumericalExpression[B]) = gte(b)
  def < [B](b: NumericalExpression[B]) = lt(b)
  def <=[B](b: NumericalExpression[B]) = lte(b)

  def +[B](b: NumericalExpression[B]) = plus(b)
  def *[B](b: NumericalExpression[B]) = times(b)
  def -[B](b: NumericalExpression[B]) = minus(b)
  def /[B](b: NumericalExpression[B]) = div(b)

  def gt [B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">")
  def gte[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">=")
  def lt [B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<")
  def lte[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<=")

  def plus[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "+")
  def times[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "*")
  def minus[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "-")
  def div[B](b: NumericalExpression[B]) = new BinaryDivOp[A,B](this, b, "/")

  def ||[B](e: TypedExpressionNode[B]) = new ConcatOp(this,e)

  def isNull = new PostfixOperatorNode("is null", this) with LogicalBoolean

  def isNotNull = new PostfixOperatorNode("is not null", this) with LogicalBoolean

  def in[B <% NumericalExpression[_]](e: RightHandSideOfIn[B]) = new InclusionOperator(this, e.toIn)
  def notIn[B <% NumericalExpression[_]](e: RightHandSideOfIn[B]) = new ExclusionOperator(this, e.toNotIn)

  def between[B,C](b: NumericalExpression[B], c: NumericalExpression[C]) = new BetweenExpression(this, b, c)

  def is(columnAttributes: AttributeValidOnNumericalColumn*)(implicit restrictUsageWithinSchema: Schema) =
    new ColumnAttributeAssignment(_fieldMetaData, columnAttributes)
  
  def ~ = this
}

trait NonNumericalExpression[A] extends TypedExpressionNode[A] {

  def ===(b: NonNumericalExpression[A]) = new EqualityExpression(this, b)
  def <>(b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<>")

  def > (b: NonNumericalExpression[A]) = gt(b)
  def >=(b: NonNumericalExpression[A]) = gte(b)
  def < (b: NonNumericalExpression[A]) =  lt(b)
  def <=(b: NonNumericalExpression[A]) = lte(b)

  def gt (b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">")
  def gte(b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">=")
  def lt (b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<")
  def lte(b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<=")

  def ||[B](e: TypedExpressionNode[B]) = new ConcatOp(this,e)

  def isNull = new PostfixOperatorNode("is null", this) with LogicalBoolean

  def isNotNull = new PostfixOperatorNode("is not null", this) with LogicalBoolean

  def in(e: RightHandSideOfIn[A]) = new InclusionOperator(this, e.toIn)
  def notIn(e: RightHandSideOfIn[A]) = new ExclusionOperator(this, e.toNotIn)

  def between(b: NonNumericalExpression[A], c: NonNumericalExpression[A]) = new BetweenExpression(this, b, c)

  def is(columnAttributes: AttributeValidOnNonNumericalColumn*)(implicit restrictUsageWithinSchema: Schema) =
    new ColumnAttributeAssignment(_fieldMetaData, columnAttributes)
}

trait BooleanExpression[A] extends NonNumericalExpression[A] {
  def ~ = this
}

trait BinaryExpression[A] extends NonNumericalExpression[A] {
  def ~ = this
}

trait EnumExpression[A] extends NonNumericalExpression[A] {
  def ~ = this
}

trait StringExpression[A] extends NonNumericalExpression[A] {
  outer =>
  
  def like(e: StringExpression[_])  = new BinaryOperatorNodeLogicalBoolean(this, e, "like")

  def regex(pattern: String) = new FunctionNode(pattern, this) with LogicalBoolean {

    override def doWrite(sw: StatementWriter) =
      Session.currentSession.databaseAdapter.writeRegexExpression(outer, pattern, sw)
  }
  
  def ~ = this
}

trait DateExpression[A] extends NonNumericalExpression[A] {

  def ~ = this
}

trait UuidExpression[A] extends NonNumericalExpression[A] {
  def ~ = this
}
