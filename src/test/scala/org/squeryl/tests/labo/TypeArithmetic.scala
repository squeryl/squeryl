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
package org.squeryl.tests.labo


object TypeArithmetic {

  type ByteType = Byte
  type IntType = Int
  type LongType = Long
  type FloatType = Float
  type DoubleType = Double


  class NumericalExpression[A] {

    def +[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "+")
    def *[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "+")

    def /[B](b: NumericalExpression[B]) = new BinaryDivOp[A,B](this, b, "/")

    def ===(s: NumericalExpression[_]) = new LBool

    def ~ = this
  }

  trait ExpressionN
  
  class NumericalTypeConvertion[A](e: ExpressionN) extends NumericalExpression[A]

  class BinaryAMSOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends ExpressionN

  class BinaryDivOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends ExpressionN

  class UnaryFloatOp[A](a: NumericalExpression[A], op: String) extends ExpressionN

  class UnaryAgregateFloatOp[A](a: NumericalExpression[A], op: String) extends ExpressionN
  
  class UnaryAgregateLengthNeutralOp[A](a: NumericalExpression[A], op: String) extends ExpressionN



  // conversions for binary ops like Addition subtraction, multiplication :
  implicit def binaryOp2TE1(op: BinaryAMSOp[ByteType,ByteType]) = new NumericalTypeConvertion[ByteType](op)
  implicit def binaryOp2TE2(op: BinaryAMSOp[ByteType,IntType]) = new NumericalTypeConvertion[IntType](op)
  implicit def binaryOp2TE3(op: BinaryAMSOp[ByteType,LongType]) = new NumericalTypeConvertion[LongType](op)
  implicit def binaryOp2TE4(op: BinaryAMSOp[ByteType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE5(op: BinaryAMSOp[ByteType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE6(op: BinaryAMSOp[ByteType,Option[ByteType]]) = new NumericalTypeConvertion[Option[ByteType]](op)
  implicit def binaryOp2TE7(op: BinaryAMSOp[ByteType,Option[IntType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE8(op: BinaryAMSOp[ByteType,Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE9(op: BinaryAMSOp[ByteType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE10(op: BinaryAMSOp[ByteType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE11(op: BinaryAMSOp[IntType,ByteType]) = new NumericalTypeConvertion[IntType](op)
  implicit def binaryOp2TE12(op: BinaryAMSOp[IntType,IntType]) = new NumericalTypeConvertion[IntType](op)
  implicit def binaryOp2TE13(op: BinaryAMSOp[IntType,LongType]) = new NumericalTypeConvertion[LongType](op)
  implicit def binaryOp2TE14(op: BinaryAMSOp[IntType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE15(op: BinaryAMSOp[IntType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE16(op: BinaryAMSOp[IntType,Option[ByteType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE17(op: BinaryAMSOp[IntType,Option[IntType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE18(op: BinaryAMSOp[IntType,Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE19(op: BinaryAMSOp[IntType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE20(op: BinaryAMSOp[IntType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE21(op: BinaryAMSOp[LongType,ByteType]) = new NumericalTypeConvertion[LongType](op)
  implicit def binaryOp2TE22(op: BinaryAMSOp[LongType,IntType]) = new NumericalTypeConvertion[LongType](op)
  implicit def binaryOp2TE23(op: BinaryAMSOp[LongType,LongType]) = new NumericalTypeConvertion[LongType](op)
  implicit def binaryOp2TE24(op: BinaryAMSOp[LongType,FloatType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE25(op: BinaryAMSOp[LongType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE26(op: BinaryAMSOp[LongType,Option[ByteType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE27(op: BinaryAMSOp[LongType,Option[IntType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE28(op: BinaryAMSOp[LongType,Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE29(op: BinaryAMSOp[LongType,Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE30(op: BinaryAMSOp[LongType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE31(op: BinaryAMSOp[FloatType,ByteType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE32(op: BinaryAMSOp[FloatType,IntType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE33(op: BinaryAMSOp[FloatType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE34(op: BinaryAMSOp[FloatType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE35(op: BinaryAMSOp[FloatType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE36(op: BinaryAMSOp[FloatType,Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE37(op: BinaryAMSOp[FloatType,Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE38(op: BinaryAMSOp[FloatType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE39(op: BinaryAMSOp[FloatType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE40(op: BinaryAMSOp[FloatType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE41(op: BinaryAMSOp[DoubleType,ByteType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE42(op: BinaryAMSOp[DoubleType,IntType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE43(op: BinaryAMSOp[DoubleType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE44(op: BinaryAMSOp[DoubleType,FloatType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE45(op: BinaryAMSOp[DoubleType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE46(op: BinaryAMSOp[DoubleType,Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE47(op: BinaryAMSOp[DoubleType,Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE48(op: BinaryAMSOp[DoubleType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE49(op: BinaryAMSOp[DoubleType,Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE50(op: BinaryAMSOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE51(op: BinaryAMSOp[Option[ByteType],ByteType]) = new NumericalTypeConvertion[Option[ByteType]](op)
  implicit def binaryOp2TE52(op: BinaryAMSOp[Option[ByteType],IntType]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE53(op: BinaryAMSOp[Option[ByteType],LongType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE54(op: BinaryAMSOp[Option[ByteType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE55(op: BinaryAMSOp[Option[ByteType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE56(op: BinaryAMSOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConvertion[Option[ByteType]](op)
  implicit def binaryOp2TE57(op: BinaryAMSOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE58(op: BinaryAMSOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE59(op: BinaryAMSOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE60(op: BinaryAMSOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE61(op: BinaryAMSOp[Option[IntType],ByteType]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE62(op: BinaryAMSOp[Option[IntType],IntType]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE63(op: BinaryAMSOp[Option[IntType],LongType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE64(op: BinaryAMSOp[Option[IntType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE65(op: BinaryAMSOp[Option[IntType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE66(op: BinaryAMSOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE67(op: BinaryAMSOp[Option[IntType],Option[IntType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def binaryOp2TE68(op: BinaryAMSOp[Option[IntType],Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE69(op: BinaryAMSOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE70(op: BinaryAMSOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE71(op: BinaryAMSOp[Option[LongType],ByteType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE72(op: BinaryAMSOp[Option[LongType],IntType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE73(op: BinaryAMSOp[Option[LongType],LongType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE74(op: BinaryAMSOp[Option[LongType],FloatType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE75(op: BinaryAMSOp[Option[LongType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE76(op: BinaryAMSOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE77(op: BinaryAMSOp[Option[LongType],Option[IntType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE78(op: BinaryAMSOp[Option[LongType],Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def binaryOp2TE79(op: BinaryAMSOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE80(op: BinaryAMSOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE81(op: BinaryAMSOp[Option[FloatType],ByteType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE82(op: BinaryAMSOp[Option[FloatType],IntType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE83(op: BinaryAMSOp[Option[FloatType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE84(op: BinaryAMSOp[Option[FloatType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE85(op: BinaryAMSOp[Option[FloatType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE86(op: BinaryAMSOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE87(op: BinaryAMSOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE88(op: BinaryAMSOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE89(op: BinaryAMSOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE90(op: BinaryAMSOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE91(op: BinaryAMSOp[Option[DoubleType],ByteType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE92(op: BinaryAMSOp[Option[DoubleType],IntType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE93(op: BinaryAMSOp[Option[DoubleType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE94(op: BinaryAMSOp[Option[DoubleType],FloatType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE95(op: BinaryAMSOp[Option[DoubleType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE96(op: BinaryAMSOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE97(op: BinaryAMSOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE98(op: BinaryAMSOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE99(op: BinaryAMSOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE100(op: BinaryAMSOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  // conversions for binary ops like Division :
  implicit def binaryOp2TE1(op: BinaryDivOp[ByteType,ByteType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE2(op: BinaryDivOp[ByteType,IntType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE3(op: BinaryDivOp[ByteType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE4(op: BinaryDivOp[ByteType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE5(op: BinaryDivOp[ByteType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE6(op: BinaryDivOp[ByteType,Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE7(op: BinaryDivOp[ByteType,Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE8(op: BinaryDivOp[ByteType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE9(op: BinaryDivOp[ByteType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE10(op: BinaryDivOp[ByteType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE11(op: BinaryDivOp[IntType,ByteType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE12(op: BinaryDivOp[IntType,IntType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE13(op: BinaryDivOp[IntType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE14(op: BinaryDivOp[IntType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE15(op: BinaryDivOp[IntType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE16(op: BinaryDivOp[IntType,Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE17(op: BinaryDivOp[IntType,Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE18(op: BinaryDivOp[IntType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE19(op: BinaryDivOp[IntType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE20(op: BinaryDivOp[IntType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE21(op: BinaryDivOp[LongType,ByteType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE22(op: BinaryDivOp[LongType,IntType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE23(op: BinaryDivOp[LongType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE24(op: BinaryDivOp[LongType,FloatType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE25(op: BinaryDivOp[LongType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE26(op: BinaryDivOp[LongType,Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE27(op: BinaryDivOp[LongType,Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE28(op: BinaryDivOp[LongType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE29(op: BinaryDivOp[LongType,Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE30(op: BinaryDivOp[LongType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE31(op: BinaryDivOp[FloatType,ByteType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE32(op: BinaryDivOp[FloatType,IntType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE33(op: BinaryDivOp[FloatType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE34(op: BinaryDivOp[FloatType,FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def binaryOp2TE35(op: BinaryDivOp[FloatType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE36(op: BinaryDivOp[FloatType,Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE37(op: BinaryDivOp[FloatType,Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE38(op: BinaryDivOp[FloatType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE39(op: BinaryDivOp[FloatType,Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE40(op: BinaryDivOp[FloatType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE41(op: BinaryDivOp[DoubleType,ByteType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE42(op: BinaryDivOp[DoubleType,IntType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE43(op: BinaryDivOp[DoubleType,LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE44(op: BinaryDivOp[DoubleType,FloatType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE45(op: BinaryDivOp[DoubleType,DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def binaryOp2TE46(op: BinaryDivOp[DoubleType,Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE47(op: BinaryDivOp[DoubleType,Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE48(op: BinaryDivOp[DoubleType,Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE49(op: BinaryDivOp[DoubleType,Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE50(op: BinaryDivOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE51(op: BinaryDivOp[Option[ByteType],ByteType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE52(op: BinaryDivOp[Option[ByteType],IntType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE53(op: BinaryDivOp[Option[ByteType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE54(op: BinaryDivOp[Option[ByteType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE55(op: BinaryDivOp[Option[ByteType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE56(op: BinaryDivOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE57(op: BinaryDivOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE58(op: BinaryDivOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE59(op: BinaryDivOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE60(op: BinaryDivOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE61(op: BinaryDivOp[Option[IntType],ByteType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE62(op: BinaryDivOp[Option[IntType],IntType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE63(op: BinaryDivOp[Option[IntType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE64(op: BinaryDivOp[Option[IntType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE65(op: BinaryDivOp[Option[IntType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE66(op: BinaryDivOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE67(op: BinaryDivOp[Option[IntType],Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE68(op: BinaryDivOp[Option[IntType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE69(op: BinaryDivOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE70(op: BinaryDivOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE71(op: BinaryDivOp[Option[LongType],ByteType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE72(op: BinaryDivOp[Option[LongType],IntType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE73(op: BinaryDivOp[Option[LongType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE74(op: BinaryDivOp[Option[LongType],FloatType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE75(op: BinaryDivOp[Option[LongType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE76(op: BinaryDivOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE77(op: BinaryDivOp[Option[LongType],Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE78(op: BinaryDivOp[Option[LongType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE79(op: BinaryDivOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE80(op: BinaryDivOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE81(op: BinaryDivOp[Option[FloatType],ByteType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE82(op: BinaryDivOp[Option[FloatType],IntType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE83(op: BinaryDivOp[Option[FloatType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE84(op: BinaryDivOp[Option[FloatType],FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE85(op: BinaryDivOp[Option[FloatType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE86(op: BinaryDivOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE87(op: BinaryDivOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE88(op: BinaryDivOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE89(op: BinaryDivOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def binaryOp2TE90(op: BinaryDivOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE91(op: BinaryDivOp[Option[DoubleType],ByteType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE92(op: BinaryDivOp[Option[DoubleType],IntType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE93(op: BinaryDivOp[Option[DoubleType],LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE94(op: BinaryDivOp[Option[DoubleType],FloatType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE95(op: BinaryDivOp[Option[DoubleType],DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE96(op: BinaryDivOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE97(op: BinaryDivOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE98(op: BinaryDivOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE99(op: BinaryDivOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def binaryOp2TE100(op: BinaryDivOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  // conversions for unary ops like Sin, Log(n,X) :
  implicit def unaryOp2TE1(op: UnaryFloatOp[ByteType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def unaryOp2TE2(op: UnaryFloatOp[IntType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def unaryOp2TE3(op: UnaryFloatOp[LongType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def unaryOp2TE4(op: UnaryFloatOp[FloatType]) = new NumericalTypeConvertion[FloatType](op)
  implicit def unaryOp2TE5(op: UnaryFloatOp[DoubleType]) = new NumericalTypeConvertion[DoubleType](op)
  implicit def unaryOp2TE6(op: UnaryFloatOp[Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE7(op: UnaryFloatOp[Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE8(op: UnaryFloatOp[Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def unaryOp2TE9(op: UnaryFloatOp[Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE10(op: UnaryFloatOp[Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  // conversions for unary ops like Avg, Stdev :
  implicit def unaryOp2TE1(op: UnaryAgregateFloatOp[ByteType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE2(op: UnaryAgregateFloatOp[IntType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE3(op: UnaryAgregateFloatOp[LongType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def unaryOp2TE4(op: UnaryAgregateFloatOp[FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE5(op: UnaryAgregateFloatOp[DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def unaryOp2TE6(op: UnaryAgregateFloatOp[Option[ByteType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE7(op: UnaryAgregateFloatOp[Option[IntType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE8(op: UnaryAgregateFloatOp[Option[LongType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def unaryOp2TE9(op: UnaryAgregateFloatOp[Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE10(op: UnaryAgregateFloatOp[Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  // conversions for unary ops like Min, Max :
  implicit def unaryOp2TE1(op: UnaryAgregateLengthNeutralOp[ByteType]) = new NumericalTypeConvertion[Option[ByteType]](op)
  implicit def unaryOp2TE2(op: UnaryAgregateLengthNeutralOp[IntType]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def unaryOp2TE3(op: UnaryAgregateLengthNeutralOp[LongType]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def unaryOp2TE4(op: UnaryAgregateLengthNeutralOp[FloatType]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE5(op: UnaryAgregateLengthNeutralOp[DoubleType]) = new NumericalTypeConvertion[Option[DoubleType]](op)
  implicit def unaryOp2TE6(op: UnaryAgregateLengthNeutralOp[Option[ByteType]]) = new NumericalTypeConvertion[Option[ByteType]](op)
  implicit def unaryOp2TE7(op: UnaryAgregateLengthNeutralOp[Option[IntType]]) = new NumericalTypeConvertion[Option[IntType]](op)
  implicit def unaryOp2TE8(op: UnaryAgregateLengthNeutralOp[Option[LongType]]) = new NumericalTypeConvertion[Option[LongType]](op)
  implicit def unaryOp2TE9(op: UnaryAgregateLengthNeutralOp[Option[FloatType]]) = new NumericalTypeConvertion[Option[FloatType]](op)
  implicit def unaryOp2TE10(op: UnaryAgregateLengthNeutralOp[Option[DoubleType]]) = new NumericalTypeConvertion[Option[DoubleType]](op)

  implicit def i2s(i:Int) = new NumericalExpression[IntType]
  implicit def f2s(i:Long) = new NumericalExpression[LongType]
  implicit def l2s(i:Float) = new NumericalExpression[FloatType]

  class LBool {
    def and(b: LBool) = new LBool
  }


  class TypeDescription(val token: String, val length:Int, val isFloat: Boolean, val isOption: Boolean) {

    override def toString =
      if(isOption)
        "Option[" + token + "Type]"
      else
        token

    def sample =
      if(isOption)
        "Some(sample" + token + ")"
      else
        "sample" + token
  }

  val tByte    = new TypeDescription("Byte", 1, false, false)
  val tInt     = new TypeDescription("Int", 4, false, false)
  val tLong    = new TypeDescription("Long", 8, false, false)
  val tFloat   = new TypeDescription("Float", 4, true, false)
  val tDouble  = new TypeDescription("Double", 8, true, false)
  
  val tByteO   = new TypeDescription("Byte", 1, false, true)
  val tIntO    = new TypeDescription("Int", 4, false, true)
  val tLongO   = new TypeDescription("Long", 8, false, true)
  val tFloatO  = new TypeDescription("Float", 4, true, true)
  val tDoubleO = new TypeDescription("Double", 8, true, true)


  def allNumericTypes =
    List(tByte,tInt,tLong,tFloat,tDouble,
         tByteO,tIntO,tLongO,tFloatO,tDoubleO) 

  def printBinaryTypeConversions(inputParamTypeName : String, outTypeFunc: (TypeDescription,TypeDescription) => TypeDescription) = {
    var i = 1
    for(t1 <- allNumericTypes;
        t2 <- allNumericTypes) {
      val outType = outTypeFunc(t1,t2)
      println("  implicit def binaryOp2TE"+i+
              "(op: "+inputParamTypeName+"[" + t1 + "," + t2 +
              "]) = new NumericalTypeConversion["+outType+"](op," + outType.sample + ")")
      i += 1
    }
  }

  def printUnaryTypeConversions(inputParamTypeName : String, outTypeFunc: (TypeDescription) => TypeDescription) = {
    var i = 1
    for(t1 <- allNumericTypes) {
      val outType = outTypeFunc(t1) 
      println("  implicit def unaryOp2TE"+i+
              "(op: "+inputParamTypeName+"[" + t1 + 
              "]) = new NumericalTypeConversion["+outType+"](op)," + outType.sample + ")")
      i += 1
    }
  }

  val computeTypeASM = (t1: TypeDescription, t2: TypeDescription) => {

    val maxLength = if(t1.length > t2.length) t1.length else t2.length
    val isOption = t1.isOption || t2.isOption
    val isFloat = t1.isFloat || t2.isFloat

    (maxLength, isOption, isFloat) match {
      case (1, false, false) => tByte
      case (1, true,  false) => tByteO
      case (1, a:Any,   b:Any)   => invalidTypeCombination(t1,t2)
      case (4, false, false) => tInt
      case (4, true,  false) => tIntO
      case (4, false, true)  => tFloat
      case (4, true,  true)  => tFloatO
      case (8, false, false) => tLong
      case (8, true,  false) => tLongO
      case (8, false, true)  => tDouble
      case (8, true,  true)  => tDoubleO
      case a:Any => invalidTypeCombination(t1,t2)
    }
  }

  val computeTypeDIV = (t1: TypeDescription, t2: TypeDescription) => {  

    val maxLength = if(t1.length > t2.length) t1.length else t2.length
    val isOption = t1.isOption || t2.isOption

    (maxLength, isOption) match {
      case (1, false) => tFloat
      case (1, true) => tFloatO
      case (4, false)  => tFloat
      case (4, true)  => tFloatO
      case (8, false)  => tDouble
      case (8, true)  => tDoubleO
      case a:Any => invalidTypeCombination(t1,t2)
    }
  }

  // for functions like Log(n,X), Sin(x), etc
  val computeTypeFloatOp = (t1: TypeDescription) => {  

    (t1.length, t1.isOption) match {
      case (1, false) => tFloat
      case (1, true) => tFloatO
      case (4, false)  => tFloat
      case (4, true)  => tFloatO
      case (8, false)  => tDouble
      case (8, true)  => tDoubleO
    }
  }

  //for aggregate functions like Avg, Stdev, etc...
  val computeTypeAgregateFloatOp = (t1: TypeDescription) => {

    t1.length match {
      case 1 => tFloatO
      case 4  => tFloatO
      case 8  => tDoubleO
    }
  }

  val computeTypeAgregateLengthNeutralOp = (t1: TypeDescription) => {

    (t1.length, t1.isFloat) match {
      case (1, false) => tByteO
      case (1, true) => error("!!!")
      case (4, false)  => tIntO
      case (4, true)  => tFloatO
      case (8, false)  => tLongO
      case (8, true)  => tDoubleO
    }
  }
  
  def invalidTypeCombination(t1: TypeDescription,t2: TypeDescription) =
    error("invalidTypeCombination(" + t1 + "," + t2)
  
  def mainz(args : Array[String]) : Unit = {

    println("  // conversions for binary ops like Addition subtraction, multiplication :")
    printBinaryTypeConversions("BinaryAMSOp", computeTypeASM)
    println("  // conversions for binary ops like Division :")
    printBinaryTypeConversions("BinaryDivOp", computeTypeDIV)
    println("  // conversions for unary ops like Sin, Log(n,X) :")
    printUnaryTypeConversions("UnaryFloatOp", computeTypeFloatOp)
    println("  // conversions for unary ops like Avg, Stdev :")
    printUnaryTypeConversions("UnaryAgregateFloatOp", computeTypeAgregateFloatOp)
    println("  // conversions for unary ops like Min, Max :")
    printUnaryTypeConversions("UnaryAgregateLengthNeutralOp", computeTypeAgregateLengthNeutralOp)

    val v1 = (1 ~) + 1.5F : NumericalExpression[FloatType]
    val v2 = (1 ~) + 10L;
    v2 : NumericalExpression[Long]

    (1 ~) / 4L : NumericalExpression[Double]

    (1 ~) / 4 : NumericalExpression[Float]

    34 * (1 ~) / 4L : NumericalExpression[Double]

    4 === (1 ~) + 10L and (1 ~) / 4 === (1 ~) + 10L :LBool
    println(v1)
  }  
}
