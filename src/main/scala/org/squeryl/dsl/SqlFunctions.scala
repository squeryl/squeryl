package org.squeryl.dsl

import ast.{ExpressionNode, TypedExpressionNode, TokenExpressionNode, FunctionNode}

/*


*/
trait SqlFunctions  {
  self: ExpressionDsl =>

  def max[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")
  def avg[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "avg")

  def max[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")
  
  
  def count                  = new FunctionNode("count", new TokenExpressionNode("*")) with NumericalExpression[LongType]

  def nvl[A,B](a: NumericalExpression[Option[A]], b: NumericalExpression[B]) = new NvlFunctionNumerical[Option[A],B](a,b)

  def nvl[A](a: NonNumericalExpression[Option[A]], b: NonNumericalExpression[A]) = new NvlFunctionNonNumerical[Option[A],A](a,b)
}