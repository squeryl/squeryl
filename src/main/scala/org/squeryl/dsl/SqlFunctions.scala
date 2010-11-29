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
import org.squeryl.internals.{StatementWriter, OutMapper}
/*


*/
trait SqlFunctions  {
  self: TypeArithmetic =>

  def max[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")
  def sum[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "sum")
  def avg[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "avg")

  def sDevPopulation[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "stddev_pop")
  def sDevSample[A](e: NumericalExpression[A])          = new  UnaryAgregateFloatOp[A](e, "stddev_samp")
  def varPopulation[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "var_pop")
  def varSample[A](e: NumericalExpression[A])          = new  UnaryAgregateFloatOp[A](e, "var_samp")


  def max[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")

  def count: CountFunction = count()

  def count(e: TypedExpressionNode[_]*) = new CountFunction(e, false)

  def countDistinct(e: TypedExpressionNode[_]*) = new CountFunction(e, true)

  def nvl[A,B](a: NumericalExpression[Option[A]], b: NumericalExpression[B]) = new NvlFunctionNumerical[A,B](a.asInstanceOf[NumericalExpression[A]],b)

  def nvl[A](a: NonNumericalExpression[Option[A]], b: NonNumericalExpression[A]) = new NvlFunctionNonNumerical[Option[A],A](a,b)

  def not(b: LogicalBoolean) = new FunctionNode("not", b) with LogicalBoolean

  def upper[A](s: StringExpression[A]) = new FunctionNode("upper", Some(s.mapper), Seq(s)) with StringExpression[A]

  def lower[A](s: StringExpression[A]) = new FunctionNode("lower", Some(s.mapper), Seq(s)) with StringExpression[A]


  class CountFunction(_args: Seq[ExpressionNode], isDistinct: Boolean)
    extends FunctionNode[LongType](
      "count",
      Some(createOutMapperLongType) : Option[OutMapper[LongType]],
      if(_args == Nil) Seq(new TokenExpressionNode("*")) else _args
    )
    with NumericalExpression[LongType] {

    override def doWrite(sw: StatementWriter) = {

      sw.write(name)
      sw.write("(")

      if(isDistinct)
        sw.write("distinct ")

      sw.writeNodesWithSeparator(args, ",", false)
      sw.write(")")
    }
  }
}
