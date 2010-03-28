package org.squeryl.dsl

import ast._
import org.squeryl.internals.{OutMapper}
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

  def count = new CountFunction

  def nvl[A,B](a: NumericalExpression[Option[A]], b: NumericalExpression[B]) = new NvlFunctionNumerical[A,B](a,b)

  def nvl[A](a: NonNumericalExpression[Option[A]], b: NonNumericalExpression[A]) = new NvlFunctionNonNumerical[Option[A],A](a,b)

  def not(b: LogicalBoolean) = new FunctionNode("not", b) with LogicalBoolean


  class CountFunction
    extends FunctionNode[LongType]("count", Some(createOutMapperLongType) : Option[OutMapper[LongType]], List(new TokenExpressionNode("*"))) 
      with NumericalExpression[LongType]
}