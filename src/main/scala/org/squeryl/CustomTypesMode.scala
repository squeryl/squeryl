package org.squeryl

import dsl.ast.{ConstantExpressionNode}
import dsl.QueryDsl


trait CustomType {
  def wrappedValue: Any
}

trait CustomTypesMode extends QueryDsl {

  implicit def createConstantNodeOfScalarIntType(i: Int) =
    new ConstantExpressionNode[Int](i) with NumericalExpression[Int]

  implicit def createConstantNodeOfScalarStringType(s: String) =
    new ConstantExpressionNode[String](s, true) with StringExpression[String]

  implicit def createConstantNodeOfScalarDoubleType(i: Double) =
    new ConstantExpressionNode[Double](i) with NumericalExpression[Double]

  implicit def createConstantNodeOfScalarFloatType(i: Float) =
    new ConstantExpressionNode[Float](i) with NumericalExpression[Float]

  implicit def createConstantNodeOfScalarLongType(i: Long) =
    new ConstantExpressionNode[Long](i) with NumericalExpression[Long]

  implicit def createConstantNodeOfScalarBooleanType(i: Boolean) =
    new ConstantExpressionNode[Boolean](i) with NonNumericalExpression[Boolean]

}
