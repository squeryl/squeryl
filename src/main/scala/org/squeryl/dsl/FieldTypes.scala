package org.squeryl.dsl

import ast._
import org.squeryl.internals.StatementWriter
import org.squeryl.{Session, Query}

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

  trait NumericalExpression[A] extends TypedExpressionNode[A] {

    def +[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "+")
    def *[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "*")
    def -[B](b: NumericalExpression[B]) = new BinaryAMSOp[A,B](this, b, "-")
    def /[B](b: NumericalExpression[B]) = new BinaryDivOp[A,B](this, b, "/")

    def =?[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "=")
    def <>[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<>")
    def > [B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">")
    def >=[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">=")
    def < [B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<")
    def <=[B](b: NumericalExpression[B]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<=")

    def ||[B](e: TypedExpressionNode[B]):StringExpression[B] = new ConcatFunction(List(this,e)) with StringExpression[B]

    def isNull = new PostfixOperatorNode("is null", this) with LogicalBoolean

    def isNotNull = new PostfixOperatorNode("is not null", this) with LogicalBoolean

    def in[B <% NumericalExpression[_]](e: Query[B]) = new BinaryOperatorNodeLogicalBoolean(this, e.ast, "in")

    def in(l: ListNumerical) = new BinaryOperatorNodeLogicalBoolean(this, l, "in")

    def ~ = this
  }

  trait NonNumericalExpression[A] extends TypedExpressionNode[A] {

    def =?[A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "=")
    def <>[A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<>")
    def > [A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">")
    def >=[A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">=")
    def < [A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<")
    def <=[A](b: NonNumericalExpression[A]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<=")

    def ||[B](e: TypedExpressionNode[B]):StringExpression[B] = new ConcatFunction(List(this,e)) with StringExpression[B]

    def isNull = new PostfixOperatorNode("is null", this) with LogicalBoolean

    def isNotNull = new PostfixOperatorNode("is not null", this) with LogicalBoolean

    def in[A](e: Query[A]) = new BinaryOperatorNodeLogicalBoolean(this, e.ast, "in")
  }

  trait BooleanExpression[A] extends NonNumericalExpression[A] {
    def ~ = this
  }

  trait StringExpression[A] extends NonNumericalExpression[A] {

    def in(e: ListString) = new BinaryOperatorNodeLogicalBoolean(this, e, "in")

    //def between(lower: BaseScalarString, upper: BaseScalarString): LogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with LogicalBoolean
    def like(e: StringExpression[_])  = new BinaryOperatorNodeLogicalBoolean(this, e, "like")

    def ~ = this
  }

  trait DateExpression[A] extends NonNumericalExpression[A] {
    def ~ = this
  }

  private class ConcatFunction(e: Iterable[ExpressionNode]) extends FunctionNode("concat",e) {
    override def write(sw: StatementWriter) = {
      val s = Session.currentSession
      s.databaseAdapter.writeConcatFunctionCall(this, sw)
    }
  }
  
}