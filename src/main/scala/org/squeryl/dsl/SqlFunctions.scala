package org.squeryl.dsl

import ast._
import org.squeryl.internals.{NoOpOutMapper, OutMapper}
/*


*/
trait SqlFunctions  {
  self: TypeArithmetic =>

  def max[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")
  def avg[A](e: NumericalExpression[A])      = new  UnaryAgregateFloatOp[A](e, "avg")

  def max[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "max")
  def min[A](e: NonNumericalExpression[A])      = new  UnaryAgregateLengthNeutralOp[A](e, "min")

  def count = new CountFunction

  def nvl[A,B](a: NumericalExpression[Option[A]], b: NumericalExpression[B]) = new NvlFunctionNumerical[A,B](a,b)

  def nvl[A](a: NonNumericalExpression[Option[A]], b: NonNumericalExpression[A]) = new NvlFunctionNonNumerical[Option[A],A](a,b)

  def not(b: LogicalBoolean) = new FunctionNode("not", b) with LogicalBoolean


  class CountFunction
    extends FunctionNode[LongType]("count", Some(createOutMapperLongType) : Option[OutMapper[LongType]], List(new TokenExpressionNode("*"))) 
      with NumericalExpression[LongType]
}