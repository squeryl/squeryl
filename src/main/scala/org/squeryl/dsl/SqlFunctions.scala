package org.squeryl.dsl

import ast._

/*


*/
trait SqlFunctions  {
  self: TypeArithmetic =>

  def max[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")
  def avg[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "avg")

  def max[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")

  def count = new FunctionNode[LongType]("count", sampleLong, new TokenExpressionNode("*")) with NumericalExpression[LongType]

  def nvl[A,B](a: NumericalExpression[Option[A]], b: NumericalExpression[B]) = new NvlFunctionNumerical[A,B](a,b)

  def nvl[A](a: NonNumericalExpression[Option[A]], b: NonNumericalExpression[A]) = new NvlFunctionNonNumerical[Option[A],A](a,b)

  def not(b: LogicalBoolean) = new FunctionNode("not", b) with LogicalBoolean
}