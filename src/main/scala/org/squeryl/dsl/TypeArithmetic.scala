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
import org.squeryl.internals._
import java.util.Date
import java.sql.{Timestamp, ResultSet}

class NumericalTypeConversion[A](e: ExpressionNode)(implicit val mapper: OutMapper[A]) extends TypeConversion(e) with NumericalExpression[A]

class DateTypeConversion[A](e: ExpressionNode)(implicit val mapper: OutMapper[A]) extends TypeConversion(e) with DateExpression[A]

class StringTypeConversion[A](e: ExpressionNode)(implicit val mapper: OutMapper[A]) extends TypeConversion(e) with StringExpression[A]

class BooleanTypeConversion[A](e: ExpressionNode)(implicit val mapper: OutMapper[A]) extends TypeConversion(e) with BooleanExpression[A]

class BinaryAMSOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends BinaryOperatorNode(a1,a2, op)

class ConcatOp[A1,A2](a1: TypedExpressionNode[A1], a2: TypedExpressionNode[A2]) extends BinaryOperatorNode(a1,a2, "||") {
  override def doWrite(sw: StatementWriter) =
      sw.databaseAdapter.writeConcatOperator(a1, a2, sw)
}

class NonNumericalCoalesce[A1,A2](val a1: NonNumericalExpression[A1], val a2: NonNumericalExpression[A2], op: String) extends BinaryOperatorNode(a1,a2, op)

class NonNumericalTypeConversion[A](e: ExpressionNode)(implicit val mapper: OutMapper[A]) extends TypeConversion(e) with NonNumericalExpression[A]

/**
 * SQL allows operators are 'null agnostic', i.e. where a.z = 4 is valid
 * even if z is Option[Int]. this class is meant for conversions like :
 *  
 *   NonNumericalExpression[Option[A]]) --> NonNumericalExpression[A]
 */
class NonNumericalInputOnlyTypeConversion[A](e: ExpressionNode) extends TypeConversion(e) with NonNumericalExpression[A] {
   override def mapper: OutMapper[A] = error(
      "Bug ! implicit conversion 'emulateSqlTyping1' is not supposed to get triggered in AST nodes participating in ResulSet extraction")
}

class NumericalInputOnlyTypeConversion[A](e: ExpressionNode) extends TypeConversion(e) with NumericalExpression[A] {
   override def mapper: OutMapper[A] = error(
      "Bug ! implicit conversion 'emulateSqlTyping1' is not supposed to get triggered in AST nodes participating in ResulSet extraction")
}


trait NvlNode {
  self: BinaryOperatorNode =>

  override def doWrite(sw: StatementWriter) =
    sw.databaseAdapter.writeNvlCall(left, right, sw)
}

class NvlFunctionNonNumerical[A1,A2](a1: NonNumericalExpression[A1], a2: NonNumericalExpression[A2])
  extends BinaryOperatorNode(a1,a2, "nvl") with NvlNode

class NvlFunctionNumerical[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2])
  extends BinaryAMSOp[A1,A2](a1,a2, "nvl")  with NvlNode

class BinaryDivOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends BinaryOperatorNode(a1,a2, op)

class UnaryFloatOp[A](a: NumericalExpression[A], op: String) extends FunctionNode(op, a)

class UnaryAgregateFloatOp[A](a: NumericalExpression[A], op: String) extends FunctionNode(op, a)

class UnaryAgregateLengthNeutralOp[A](val a: TypedExpressionNode[A], op: String) extends FunctionNode(op, a)


trait TypeArithmetic extends FieldTypes {


  // conversions for binary ops like Addition subtraction, multiplication :
  implicit def binaryOpConv1(op: BinaryAMSOp[ByteType,ByteType]) = new NumericalTypeConversion[ByteType](op)
  implicit def binaryOpConv2(op: BinaryAMSOp[ByteType,IntType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv3(op: BinaryAMSOp[ByteType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv4(op: BinaryAMSOp[ByteType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv5(op: BinaryAMSOp[ByteType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv5bd(op: BinaryAMSOp[ByteType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv6(op: BinaryAMSOp[ByteType,Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv7(op: BinaryAMSOp[ByteType,Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv8(op: BinaryAMSOp[ByteType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv9(op: BinaryAMSOp[ByteType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv10(op: BinaryAMSOp[ByteType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv10bd(op: BinaryAMSOp[ByteType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv11(op: BinaryAMSOp[IntType,ByteType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv12(op: BinaryAMSOp[IntType,IntType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv13(op: BinaryAMSOp[IntType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv14(op: BinaryAMSOp[IntType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv15(op: BinaryAMSOp[IntType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv15bd(op: BinaryAMSOp[IntType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv16(op: BinaryAMSOp[IntType,Option[ByteType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv17(op: BinaryAMSOp[IntType,Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv18(op: BinaryAMSOp[IntType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv19(op: BinaryAMSOp[IntType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv20(op: BinaryAMSOp[IntType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv20bd(op: BinaryAMSOp[IntType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv21(op: BinaryAMSOp[LongType,ByteType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv22(op: BinaryAMSOp[LongType,IntType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv23(op: BinaryAMSOp[LongType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv24(op: BinaryAMSOp[LongType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25(op: BinaryAMSOp[LongType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25bd(op: BinaryAMSOp[LongType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv26(op: BinaryAMSOp[LongType,Option[ByteType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv27(op: BinaryAMSOp[LongType,Option[IntType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv28(op: BinaryAMSOp[LongType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv29(op: BinaryAMSOp[LongType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30(op: BinaryAMSOp[LongType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30bd(op: BinaryAMSOp[LongType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv31(op: BinaryAMSOp[FloatType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv32(op: BinaryAMSOp[FloatType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv33(op: BinaryAMSOp[FloatType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv34(op: BinaryAMSOp[FloatType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv35(op: BinaryAMSOp[FloatType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv35bd(op: BinaryAMSOp[FloatType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv36(op: BinaryAMSOp[FloatType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv37(op: BinaryAMSOp[FloatType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv38(op: BinaryAMSOp[FloatType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv39(op: BinaryAMSOp[FloatType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv40(op: BinaryAMSOp[FloatType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv40bd(op: BinaryAMSOp[FloatType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv41(op: BinaryAMSOp[DoubleType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv42(op: BinaryAMSOp[DoubleType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv43(op: BinaryAMSOp[DoubleType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv44(op: BinaryAMSOp[DoubleType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45(op: BinaryAMSOp[DoubleType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45bd(op: BinaryAMSOp[DoubleType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv46(op: BinaryAMSOp[DoubleType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv47(op: BinaryAMSOp[DoubleType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv48(op: BinaryAMSOp[DoubleType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv49(op: BinaryAMSOp[DoubleType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50(op: BinaryAMSOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50bd(op: BinaryAMSOp[DoubleType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv51(op: BinaryAMSOp[Option[ByteType],ByteType]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv52(op: BinaryAMSOp[Option[ByteType],IntType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv53(op: BinaryAMSOp[Option[ByteType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv54(op: BinaryAMSOp[Option[ByteType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv55(op: BinaryAMSOp[Option[ByteType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv55bd(op: BinaryAMSOp[Option[ByteType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv56(op: BinaryAMSOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv57(op: BinaryAMSOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv58(op: BinaryAMSOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv59(op: BinaryAMSOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv60(op: BinaryAMSOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv60bd(op: BinaryAMSOp[Option[ByteType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv61(op: BinaryAMSOp[Option[IntType],ByteType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv62(op: BinaryAMSOp[Option[IntType],IntType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv63(op: BinaryAMSOp[Option[IntType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv64(op: BinaryAMSOp[Option[IntType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv65(op: BinaryAMSOp[Option[IntType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv65bd(op: BinaryAMSOp[Option[IntType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv66(op: BinaryAMSOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv67(op: BinaryAMSOp[Option[IntType],Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv68(op: BinaryAMSOp[Option[IntType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv69(op: BinaryAMSOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv70(op: BinaryAMSOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv70bd(op: BinaryAMSOp[Option[IntType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv71(op: BinaryAMSOp[Option[LongType],ByteType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv72(op: BinaryAMSOp[Option[LongType],IntType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv73(op: BinaryAMSOp[Option[LongType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv74(op: BinaryAMSOp[Option[LongType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75(op: BinaryAMSOp[Option[LongType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75bd(op: BinaryAMSOp[Option[LongType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv76(op: BinaryAMSOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv77(op: BinaryAMSOp[Option[LongType],Option[IntType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv78(op: BinaryAMSOp[Option[LongType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv79(op: BinaryAMSOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80(op: BinaryAMSOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80bd(op: BinaryAMSOp[Option[LongType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv81(op: BinaryAMSOp[Option[FloatType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv82(op: BinaryAMSOp[Option[FloatType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv83(op: BinaryAMSOp[Option[FloatType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv84(op: BinaryAMSOp[Option[FloatType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv85(op: BinaryAMSOp[Option[FloatType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv85bd(op: BinaryAMSOp[Option[FloatType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv86(op: BinaryAMSOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv87(op: BinaryAMSOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv88(op: BinaryAMSOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv89(op: BinaryAMSOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv90(op: BinaryAMSOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv90bd(op: BinaryAMSOp[Option[FloatType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv91(op: BinaryAMSOp[Option[DoubleType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv92(op: BinaryAMSOp[Option[DoubleType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv93(op: BinaryAMSOp[Option[DoubleType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv94(op: BinaryAMSOp[Option[DoubleType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95(op: BinaryAMSOp[Option[DoubleType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95bd(op: BinaryAMSOp[Option[DoubleType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv96(op: BinaryAMSOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv97(op: BinaryAMSOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv98(op: BinaryAMSOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv99(op: BinaryAMSOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100(op: BinaryAMSOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100bd(op: BinaryAMSOp[Option[DoubleType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  // conversions for binary ops like Division :
  implicit def binaryOpConv1(op: BinaryDivOp[ByteType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv2(op: BinaryDivOp[ByteType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv3(op: BinaryDivOp[ByteType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv4(op: BinaryDivOp[ByteType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv5(op: BinaryDivOp[ByteType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv5bd(op: BinaryDivOp[ByteType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv6(op: BinaryDivOp[ByteType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv7(op: BinaryDivOp[ByteType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv8(op: BinaryDivOp[ByteType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv9(op: BinaryDivOp[ByteType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv10(op: BinaryDivOp[ByteType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv10bd(op: BinaryDivOp[ByteType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv11(op: BinaryDivOp[IntType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv12(op: BinaryDivOp[IntType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv13(op: BinaryDivOp[IntType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv14(op: BinaryDivOp[IntType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv15(op: BinaryDivOp[IntType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv15bd(op: BinaryDivOp[IntType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv16(op: BinaryDivOp[IntType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv17(op: BinaryDivOp[IntType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv18(op: BinaryDivOp[IntType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv19(op: BinaryDivOp[IntType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv20(op: BinaryDivOp[IntType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv20bd(op: BinaryDivOp[IntType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv21(op: BinaryDivOp[LongType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv22(op: BinaryDivOp[LongType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv23(op: BinaryDivOp[LongType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv24(op: BinaryDivOp[LongType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25(op: BinaryDivOp[LongType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25bd(op: BinaryDivOp[LongType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv26(op: BinaryDivOp[LongType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv27(op: BinaryDivOp[LongType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv28(op: BinaryDivOp[LongType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv29(op: BinaryDivOp[LongType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30(op: BinaryDivOp[LongType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30bd(op: BinaryDivOp[LongType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv31(op: BinaryDivOp[FloatType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv32(op: BinaryDivOp[FloatType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv33(op: BinaryDivOp[FloatType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv34(op: BinaryDivOp[FloatType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv35(op: BinaryDivOp[FloatType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv35bd(op: BinaryDivOp[FloatType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv36(op: BinaryDivOp[FloatType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv37(op: BinaryDivOp[FloatType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv38(op: BinaryDivOp[FloatType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv39(op: BinaryDivOp[FloatType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv40(op: BinaryDivOp[FloatType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv40bd(op: BinaryDivOp[FloatType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv41(op: BinaryDivOp[DoubleType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv42(op: BinaryDivOp[DoubleType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv43(op: BinaryDivOp[DoubleType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv44(op: BinaryDivOp[DoubleType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45(op: BinaryDivOp[DoubleType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45bd(op: BinaryDivOp[DoubleType,BigDecimalType]) = new NumericalTypeConversion[BigDecimalType](op)
  implicit def binaryOpConv46(op: BinaryDivOp[DoubleType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv47(op: BinaryDivOp[DoubleType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv48(op: BinaryDivOp[DoubleType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv49(op: BinaryDivOp[DoubleType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50(op: BinaryDivOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50bd(op: BinaryDivOp[DoubleType,Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv51(op: BinaryDivOp[Option[ByteType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv52(op: BinaryDivOp[Option[ByteType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv53(op: BinaryDivOp[Option[ByteType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv54(op: BinaryDivOp[Option[ByteType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv55(op: BinaryDivOp[Option[ByteType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv55bd(op: BinaryDivOp[Option[ByteType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv56(op: BinaryDivOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv57(op: BinaryDivOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv58(op: BinaryDivOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv59(op: BinaryDivOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv60(op: BinaryDivOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv60bd(op: BinaryDivOp[Option[ByteType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv61(op: BinaryDivOp[Option[IntType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv62(op: BinaryDivOp[Option[IntType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv63(op: BinaryDivOp[Option[IntType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv64(op: BinaryDivOp[Option[IntType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv65(op: BinaryDivOp[Option[IntType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv65bd(op: BinaryDivOp[Option[IntType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv66(op: BinaryDivOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv67(op: BinaryDivOp[Option[IntType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv68(op: BinaryDivOp[Option[IntType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv69(op: BinaryDivOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv70(op: BinaryDivOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv70bd(op: BinaryDivOp[Option[IntType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv71(op: BinaryDivOp[Option[LongType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv72(op: BinaryDivOp[Option[LongType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv73(op: BinaryDivOp[Option[LongType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv74(op: BinaryDivOp[Option[LongType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75(op: BinaryDivOp[Option[LongType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75bd(op: BinaryDivOp[Option[LongType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv76(op: BinaryDivOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv77(op: BinaryDivOp[Option[LongType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv78(op: BinaryDivOp[Option[LongType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv79(op: BinaryDivOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80(op: BinaryDivOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80bd(op: BinaryDivOp[Option[LongType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv81(op: BinaryDivOp[Option[FloatType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv82(op: BinaryDivOp[Option[FloatType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv83(op: BinaryDivOp[Option[FloatType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv84(op: BinaryDivOp[Option[FloatType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv85(op: BinaryDivOp[Option[FloatType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv85bd(op: BinaryDivOp[Option[FloatType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv86(op: BinaryDivOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv87(op: BinaryDivOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv88(op: BinaryDivOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv89(op: BinaryDivOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv90(op: BinaryDivOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv90bd(op: BinaryDivOp[Option[FloatType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv91(op: BinaryDivOp[Option[DoubleType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv92(op: BinaryDivOp[Option[DoubleType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv93(op: BinaryDivOp[Option[DoubleType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv94(op: BinaryDivOp[Option[DoubleType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95(op: BinaryDivOp[Option[DoubleType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95bd(op: BinaryDivOp[Option[DoubleType],BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def binaryOpConv96(op: BinaryDivOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv97(op: BinaryDivOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv98(op: BinaryDivOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv99(op: BinaryDivOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100(op: BinaryDivOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100bd(op: BinaryDivOp[Option[DoubleType],Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  // conversions for unary ops like Sin, Log(n,X) :
  implicit def unaryOpConv1(op: UnaryFloatOp[ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def unaryOpConv2(op: UnaryFloatOp[IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def unaryOpConv3(op: UnaryFloatOp[LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def unaryOpConv4(op: UnaryFloatOp[FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def unaryOpConv5(op: UnaryFloatOp[DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def unaryOpConv6(op: UnaryFloatOp[Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv7(op: UnaryFloatOp[Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv8(op: UnaryFloatOp[Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv9(op: UnaryFloatOp[Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv10(op: UnaryFloatOp[Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  // conversions for unary ops like Avg, Stdev :
  implicit def unaryOpConv1(op: UnaryAgregateFloatOp[ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv2(op: UnaryAgregateFloatOp[IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv3(op: UnaryAgregateFloatOp[LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv4(op: UnaryAgregateFloatOp[FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv5(op: UnaryAgregateFloatOp[DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv6(op: UnaryAgregateFloatOp[Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv7(op: UnaryAgregateFloatOp[Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv8(op: UnaryAgregateFloatOp[Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv9(op: UnaryAgregateFloatOp[Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv10(op: UnaryAgregateFloatOp[Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  // conversions for unary ops like Min, Max :
  implicit def unaryOpConv1(op: UnaryAgregateLengthNeutralOp[ByteType]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def unaryOpConv2(op: UnaryAgregateLengthNeutralOp[IntType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def unaryOpConv3(op: UnaryAgregateLengthNeutralOp[LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def unaryOpConv4(op: UnaryAgregateLengthNeutralOp[FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv5(op: UnaryAgregateLengthNeutralOp[DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv5bd(op: UnaryAgregateLengthNeutralOp[BigDecimalType]) = new NumericalTypeConversion[Option[BigDecimalType]](op)
  implicit def unaryOpConv6(op: UnaryAgregateLengthNeutralOp[Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def unaryOpConv7(op: UnaryAgregateLengthNeutralOp[Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def unaryOpConv8(op: UnaryAgregateLengthNeutralOp[Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def unaryOpConv9(op: UnaryAgregateLengthNeutralOp[Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv10(op: UnaryAgregateLengthNeutralOp[Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def unaryOpConv10bd(op: UnaryAgregateLengthNeutralOp[Option[BigDecimalType]]) = new NumericalTypeConversion[Option[BigDecimalType]](op)


  implicit def unaryOpConv11(op: UnaryAgregateLengthNeutralOp[DateType]) = new DateTypeConversion[Option[DateType]](op)
  implicit def unaryOpConv12(op: UnaryAgregateLengthNeutralOp[Option[DateType]]) = new DateTypeConversion[Option[DateType]](op)
  implicit def unaryOpConv13(op: UnaryAgregateLengthNeutralOp[StringType]) = new DateTypeConversion[Option[StringType]](op)
  implicit def unaryOpConv14(op: UnaryAgregateLengthNeutralOp[Option[StringType]]) = new DateTypeConversion[Option[StringType]](op)
  implicit def unaryOpConv15(op: UnaryAgregateLengthNeutralOp[BooleanType]) = new BooleanTypeConversion[Option[BooleanType]](op)
  implicit def unaryOpConv16(op: UnaryAgregateLengthNeutralOp[Option[BooleanType]]) = new BooleanTypeConversion[Option[BooleanType]](op)

  implicit def nvl1(e: NvlFunctionNonNumerical[Option[DateType],DateType]) = new DateTypeConversion[DateType](e)
  implicit def nvl2(e: NvlFunctionNonNumerical[Option[StringType],StringType]) = new StringTypeConversion[StringType](e)
  implicit def nvl2(e: NvlFunctionNonNumerical[Option[BooleanType],BooleanType]) = new BooleanTypeConversion[BooleanType](e)

  implicit def e2concat1[A1,A2](e: ConcatOp[A1,A2]) = new StringTypeConversion[StringType](e)(createOutMapperStringType)
  implicit def e2concat2[A1,A2](e: ConcatOp[A1,Option[A2]]) = new StringTypeConversion[Option[StringType]](e)(createOutMapperStringTypeOption)
  implicit def e2concat3[A1,A2](e: ConcatOp[Option[A1],A2]) = new StringTypeConversion[Option[StringType]](e)(createOutMapperStringTypeOption)
  implicit def e2concat4[A1,A2](e: ConcatOp[Option[A1],Option[A2]]) = new StringTypeConversion[Option[StringType]](e)(createOutMapperStringTypeOption)

  //Conversions for non numerical case statements and coalesce like functions :
  implicit def nnCoalesce1[A](e: NonNumericalCoalesce[A,A]) = new NonNumericalTypeConversion[A](e)(e.a1.mapper)
  implicit def nnCoalesce2[A](e: NonNumericalCoalesce[A,Option[A]]) = new NonNumericalTypeConversion[Option[A]](e)(e.a2.mapper)
  implicit def nnCoalesce3[A](e: NonNumericalCoalesce[Option[A],A]) = new NonNumericalTypeConversion[Option[A]](e)(e.a1.mapper)
  implicit def nnCoalesce4[A](e: NonNumericalCoalesce[Option[A],Option[A]]) = new NonNumericalTypeConversion[Option[A]](e)(e.a2.mapper)

  implicit def emulateSqlTyping1[A](e: NonNumericalExpression[Option[A]]): NonNumericalExpression[A] = new NonNumericalInputOnlyTypeConversion(e)

  implicit def emulateSqlTyping2[A](e: NumericalExpression[Option[A]]): NumericalExpression[A] = new NumericalInputOnlyTypeConversion(e)
  
  protected def mapByte2ByteType(b:Byte): ByteType
  protected def mapInt2IntType(i: Int): IntType
  protected def mapString2StringType(s: String): StringType
  protected def mapDouble2DoubleType(d: Double): DoubleType
  protected def mapBigDecimal2BigDecimalType(d: BigDecimal): BigDecimalType
  protected def mapFloat2FloatType(d: Float): FloatType
  protected def mapLong2LongType(l: Long): LongType
  protected def mapBoolean2BooleanType(b: Boolean): BooleanType
  protected def mapBinary2BinaryType(b: Array[Byte]): BinaryType
  protected def mapDate2DateType(b: Date): DateType
  protected def mapTimestamp2TimestampType(b: Timestamp): TimestampType
  //protected def mapInt2EnumerationValueType(b: Int): EnumerationValueType    

  protected implicit def createOutMapperByteType: OutMapper[ByteType] = new OutMapper[ByteType] {
    def doMap(rs: ResultSet) = mapByte2ByteType(rs.getByte(index))
    def sample = sampleByte
  }
  
  protected implicit def createOutMapperIntType: OutMapper[IntType] = new OutMapper[IntType] {
    def doMap(rs: ResultSet) = mapInt2IntType(rs.getInt(index))
    def sample = sampleInt
  }

  protected implicit def createOutMapperStringType: OutMapper[StringType] = new OutMapper[StringType] {
    def doMap(rs: ResultSet) = mapString2StringType(rs.getString(index))
    def sample = sampleString
  }

  protected implicit def createOutMapperDoubleType: OutMapper[DoubleType] = new OutMapper[DoubleType] {
    def doMap(rs: ResultSet) = mapDouble2DoubleType(rs.getDouble(index))
    def sample = sampleDouble
  }

  protected implicit def createOutMapperBigDecimalType: OutMapper[BigDecimalType] = new OutMapper[BigDecimalType] {
    def doMap(rs: ResultSet) = mapBigDecimal2BigDecimalType(new BigDecimal(rs.getBigDecimal(index)))
    def sample = sampleBigDecimal
  }

  protected implicit def createOutMapperFloatType: OutMapper[FloatType] = new OutMapper[FloatType] {
    def doMap(rs: ResultSet) = mapFloat2FloatType(rs.getFloat(index))
    def sample = sampleFloat
  }

  protected implicit def createOutMapperLongType: OutMapper[LongType] = new OutMapper[LongType] {
    def doMap(rs: ResultSet) = mapLong2LongType(rs.getLong(index))
    def sample = sampleLong
  }

  protected implicit def createOutMapperBooleanType: OutMapper[BooleanType] = new OutMapper[BooleanType] {
    def doMap(rs: ResultSet) = mapBoolean2BooleanType(rs.getBoolean(index))
    def sample = sampleBoolean
  }

  protected implicit def createOutMapperBinaryType: OutMapper[BinaryType] = new OutMapper[BinaryType] {
    def doMap(rs: ResultSet) = mapBinary2BinaryType(rs.getBytes(index))
    def sample = sampleBinary
  }

  protected implicit def createOutMapperDateType: OutMapper[DateType] = new OutMapper[DateType] {
    def doMap(rs: ResultSet) = mapDate2DateType(rs.getDate(index))
    def sample = sampleDate
  }

  protected implicit def createOutMapperTimestampType: OutMapper[TimestampType] = new OutMapper[TimestampType] {
    def doMap(rs: ResultSet) = mapTimestamp2TimestampType(rs.getTimestamp(index))
    def sample = sampleTimestamp
  }
//  protected implicit def createOutMapperEnumerationValueType: OutMapper[EnumerationValueType] = new OutMapper[EnumerationValueType] {
//    def doMap(rs: ResultSet) = mapInt2EnumerationValueType(rs.getInt(index))
//    def sample = sampleEnumerationValueType
//  }

  protected implicit def createOutMapperByteTypeOption: OutMapper[Option[ByteType]] = new OutMapper[Option[ByteType]] {
    def doMap(rs: ResultSet) = {
      val v = mapByte2ByteType(rs.getByte(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleByte)
  }

  protected implicit def createOutMapperIntTypeOption: OutMapper[Option[IntType]] = new OutMapper[Option[IntType]] {
    def doMap(rs: ResultSet) = {
      val v = mapInt2IntType(rs.getInt(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleInt)
  }

  protected implicit def createOutMapperDoubleTypeOption: OutMapper[Option[DoubleType]] = new OutMapper[Option[DoubleType]] {
    def doMap(rs: ResultSet) = {
      val v = mapDouble2DoubleType(rs.getDouble(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleDouble)
  }

  protected implicit def createOutMapperBigDecimalTypeOption: OutMapper[Option[BigDecimalType]] = new OutMapper[Option[BigDecimalType]] {
    def doMap(rs: ResultSet) = {
      val v = mapBigDecimal2BigDecimalType(new BigDecimal(rs.getBigDecimal(index)))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleBigDecimal)
  }

  protected implicit def createOutMapperFloatTypeOption: OutMapper[Option[FloatType]] = new OutMapper[Option[FloatType]] {
    def doMap(rs: ResultSet) = {
      val v = mapFloat2FloatType(rs.getFloat(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleFloat)
  }

  protected implicit def createOutMapperStringTypeOption: OutMapper[Option[StringType]] = new OutMapper[Option[StringType]] {
    def doMap(rs: ResultSet) = {
      val v = mapString2StringType(rs.getString(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleString)
  }

  protected implicit def createOutMapperLongTypeOption: OutMapper[Option[LongType]] = new OutMapper[Option[LongType]] {
    def doMap(rs: ResultSet) = {
      val v = mapLong2LongType(rs.getLong(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleLong)
  }

  protected implicit def createOutMapperBooleanTypeOption: OutMapper[Option[BooleanType]] = new OutMapper[Option[BooleanType]] {
    def doMap(rs: ResultSet) = {
      val v = mapBoolean2BooleanType(rs.getBoolean(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleBoolean)
  }

  protected implicit def createOutMapperBinaryTypeOption: OutMapper[Option[BinaryType]] = new OutMapper[Option[BinaryType]] {
    def doMap(rs: ResultSet) = {
      val v = mapBinary2BinaryType(rs.getBytes(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleBinary)
  }

  protected implicit def createOutMapperDateTypeOption: OutMapper[Option[DateType]] = new OutMapper[Option[DateType]] {
    def doMap(rs: ResultSet) = {
      val v = mapDate2DateType(rs.getDate(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleDate)
  }

  protected implicit def createOutMapperTimestampTypeOption: OutMapper[Option[TimestampType]] = new OutMapper[Option[TimestampType]] {
    def doMap(rs: ResultSet) = {
      val v = mapTimestamp2TimestampType(rs.getTimestamp(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleTimestamp)
  }  
}
