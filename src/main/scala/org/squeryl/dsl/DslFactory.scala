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
import org.squeryl.internals.{OutMapper}
import java.sql.ResultSet
import java.util.Date
import org.squeryl.Query


trait DslFactory {
  
  // List Conversion implicits don't vary with the choice of
  // column/field types, so they don't need to be overridable factory methods :
/*
  implicit def traversableOfNumericalExpressionList[A <% NumericalExpression[_]](l: Traversable[A]) =
    new RightHandSideOfIn[NumericalExpression[A]](new ConstantExpressionNodeList[Any](l))

  implicit def traversableOfEnumerationValue2ListEnumerationValue[E <: Enumeration#Value](l: Traversable[E]) = 
    new RightHandSideOfIn[E](new ConstantExpressionNodeList[E](l)) 
*/
// TODO : find out why this generalized conv for NonNumericals won't work (looks like a scalac bug...):
//  implicit def traversableOfNonNumercalExpressionList[A <% NonNumericalExpression[_]](l: Traversable[A]) =
//    new RightHandSideOfIn[NonNumericalExpression[A]](new ConstantExpressionNodeList[Any](l))

/*    
  implicit def traversableOfString2ListString(l: Traversable[StringType]) =
    new RightHandSideOfIn[StringType](new ConstantExpressionNodeList[StringType](l))

  implicit def traversableOfUuid2ListUuid(l: Traversable[UuidType]) =
    new RightHandSideOfIn[UuidType](new ConstantExpressionNodeList[UuidType](l))

  implicit def traversableOfString2OptionListString(l: Traversable[StringType]) =
    new RightHandSideOfIn[Option[StringType]](new ConstantExpressionNodeList[StringType](l))

  implicit def traversableOfDate2ListDate(l: Traversable[DateType]) =
    new RightHandSideOfIn[DateType](new ConstantExpressionNodeList[DateType](l))

  implicit def traversableOfDate2OptionListDate(l: Traversable[DateType]) =
    new RightHandSideOfIn[Option[DateType]](new ConstantExpressionNodeList[DateType](l))

  implicit def traversableOfUuidOptionList(l: Traversable[UuidType]) =
    new RightHandSideOfIn[Option[UuidType]](new ConstantExpressionNodeList[UuidType](l))
*/
  implicit def typedExpression2OrderByArg[E <% TypedExpression[_,_]](e: E) = new OrderByArg(e)

  implicit def orderByArg2OrderByExpression(a: OrderByArg) = new OrderByExpression(a)

    
  def max[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def min[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def avg[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)

  def sum[T2 >: TOption, T1 >: TNumericLowerTypeBound <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit bs: TypedExpressionFactory[A2,T2]) = bs.convert(b)
         
  def nvl[T4 <: TNonOption,
          T1 >: TOption,
          T3 >: T1,
          T2 <: T3,
          A1,A2,A3]
         (a: TypedExpression[A1,T1],
          b: TypedExpression[A2,T2])
         (implicit d: DeOptionizer[A3,T4,_,T3]): TypedExpression[A3,T4] = d.deOptionizer.convert(a)
  
  def not(b: LogicalBoolean) = new FunctionNode("not", b) with LogicalBoolean

  def upper[A1,T1](s: TypedExpression[A1,T1]) = new FunctionNode("upper", Some(s.mapper), Seq(s)) with TypedExpression[A1,T1]
  def lower[A1,T1](s: TypedExpression[A1,T1]) = new FunctionNode("lower", Some(s.mapper), Seq(s)) with TypedExpression[A1,T1]

  def exists[A1](query: Query[A1]) = new ExistsExpression(query.copy(false).ast, "exists")

  def notExists[A1](query: Query[A1]) = new ExistsExpression(query.copy(false).ast, "not exists")
         
  implicit val numericComparisonEvidence   = new CanCompare[TNumeric, TNumeric]         
  implicit val dateComparisonEvidence      = new CanCompare[TOptionDate, TOptionDate]
  implicit val timestampComparisonEvidence = new CanCompare[TOptionTimestamp, TOptionTimestamp]
  implicit val stringComparisonEvidence    = new CanCompare[TOptionString, TOptionString]
  implicit val booleanComparisonEvidence   = new CanCompare[TOptionBoolean, TOptionBoolean]
  implicit val uuidComparisonEvidence      = new CanCompare[TOptionUUID, TOptionUUID]
  implicit def enumComparisonEvidence[A]   = new CanCompare[TEnumValue[A],TEnumValue[A]]
  
  implicit def cco1[A1,A2,T1,T2](co: ConcatOp[A1,A2,T1,T2]): TypedExpression[String,TString] = sys.error("!")
  implicit def cco2[A1,A2,T1,T2](co: ConcatOp[Option[A1],A2,T1,T2]): TypedExpression[Option[String],TOptionString] = sys.error("!")
  implicit def cco3[A1,A2,T1,T2](co: ConcatOp[A1,Option[A2],T1,T2]): TypedExpression[Option[String],TOptionString] = sys.error("!")
  implicit def cco4[A1,A2,T1,T2](co: ConcatOp[Option[A1],Option[A2],T1,T2]): TypedExpression[Option[String],TOptionString] = sys.error("!")

}
