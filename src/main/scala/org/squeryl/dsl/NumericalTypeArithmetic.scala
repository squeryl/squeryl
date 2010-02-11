package org.squeryl.dsl

import ast._
import org.squeryl.internals.StatementWriter
import boilerplate._
import fsm.{QueryElements, StartState, WhereState}
import org.squeryl.internals._
import java.sql.ResultSet
import org.squeryl._

trait NumericalTypeArithmetic {

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

    def =?[B](b: NumericalExpression[B]) = new BinaryOperatorNodeScalarLogicalBoolean(this, b, "=")

    def ~ = this
  }
  
  class NumericalTypeConversion[A](e: ExpressionNode) extends NumericalExpression[A] {

    override def inhibited = e.inhibited

    override def doWrite(sw: StatementWriter)= e.doWrite((sw))

    override def children = e.children
  }

  class BinaryAMSOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends BinaryOperatorNode(a1,a2, op)

  class BinaryDivOp[A1,A2](a1: NumericalExpression[A1], a2: NumericalExpression[A2], op: String) extends BinaryOperatorNode(a1,a2, op)

  class UnaryFloatOp[A](a: NumericalExpression[A], op: String) extends FunctionNode(op, a)

  class UnaryAgregateFloatOp[A](a: NumericalExpression[A], op: String) extends FunctionNode(op, a)

  class UnaryAgregateLengthNeutralOp[A](a: NumericalExpression[A], op: String) extends FunctionNode(op, a)



  trait LogicalBooleanNode

  trait ScalarLogicalBoolean extends OpArg with TypedExpressionNode[LogicalBoolean] with LogicalBooleanNode {
    //self: ExpressionNode  =>
    def _t: ExpressionNode = this

    def and(b: ScalarLogicalBoolean) = new BinaryOperatorNodeScalarLogicalBoolean(this, b, _and)
    def or(b: ScalarLogicalBoolean) = new BinaryOperatorNodeScalarLogicalBoolean(this, b, _or)
  }


  trait LogicalBooleanInhibiter {
    self: BinaryOperatorNode =>

    override def inhibited =
      if(left.isInstanceOf[LogicalBooleanNode])
        left.inhibited && right.inhibited
      else
        left.inhibited || right.inhibited

    override def doWrite(sw: StatementWriter) = {
      // since we are executing this method, we have at least one non inhibited children
      val nonInh = children.filter(c => ! c.inhibited).iterator

      sw.write("(")
      nonInh.next.write(sw)
      sw.write(" ")
      if(nonInh.hasNext) {
        sw.write(operatorToken)
        if(newLineAfterOperator)
          sw.nextLine
        sw.write(" ")
        nonInh.next.write(sw)
      }
      sw.write(")")
    }
  }

  class BinaryOperatorNodeScalarLogicalBoolean(left: ExpressionNode, right: ExpressionNode, op: String)
    extends BinaryOperatorNode(left,right, op) with LogicalBooleanInhibiter with ScalarLogicalBoolean

  trait OpArg  {
    self: ExpressionNode =>

    protected def plus = "+"
    protected def minus = "-"
    protected def div = "/"
    protected def times = "*"
    protected def exp = "^"
    protected def eq = "="
    protected def lt = "<"
    protected def gt = ">"
    protected def in_ = "in"
    protected def _and = "and"
    protected def _or = "or"

    def isNull = new PostfixOperatorNode("is null", this) with ScalarLogicalBoolean

    def isNotNull = new PostfixOperatorNode("is not null", this) with ScalarLogicalBoolean
  }



//  trait AgregateBooleanOperators extends ExpressionNode {
//    //self: ExpressionNode =>
//
//    //def and (right: AgregateBoolean) = new BinaryOperatorNode(this, right,"and",true) with AgregateBoolean
//    //def or  (right: AgregateBoolean) = new BinaryOperatorNode(this, right,"or")  with AgregateBoolean
//  }

  //trait AgregateBoolean extends TypedExpressionNode[Agregate,BooleanType]

  trait ScalarBoolean extends TypedExpressionNode[BooleanType]

  trait ScalarBooleanOption extends TypedExpressionNode[Option[BooleanType]]

  trait NonLogicalBoolean[T] extends TypedExpressionNode[T]


  
  trait BaseScalarString extends ExpressionNode with OpArg {

    def <(e: BaseScalarString): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, lt)
    def >(e: BaseScalarString): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, gt)
    def =?(e: BaseScalarString): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, eq)
    def in(e: ListString): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e, in_)
    def in[S <% BaseScalarString](e: Query[S]): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e.ast, in_)
    def between(lower: BaseScalarString, upper: BaseScalarString): ScalarLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean
    def like(e: ScalarString): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, "like")

    def ||(e: ScalarStringOption) = new ConcatFunction(List(this,e)) with ScalarStringOption
  }

  trait ScalarString extends BaseScalarString with NonLogicalBoolean[StringType] {
    def ~ = this
    def ||(e: ScalarString)  = new ConcatFunction(List(this,e)) with ScalarString
  }

  trait ScalarStringOption extends BaseScalarString with NonLogicalBoolean[Option[StringType]] {
    def ~ = this
    def ||(e: ScalarString)  = new ConcatFunction(List(this,e)) with ScalarStringOption
  }

  trait BaseScalarDate extends ExpressionNode with OpArg {

    def <(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, lt)
    def >(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, gt)
    def <=(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, "<=")
    def >=(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, ">=")
    def =?(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, eq)
    def in[S <% BaseScalarDate](e: Query[S]): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e.ast, in_)
    def between(lower: BaseScalarDate, upper: BaseScalarDate): ScalarLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean
  }

  trait ScalarDate extends BaseScalarDate with NonLogicalBoolean[DateType] {
    def ~ = this
  }

  trait ScalarDateOption extends BaseScalarDate with NonLogicalBoolean[Option[DateType]] {
    def ~ = this
  }
  
  def not(b: ScalarLogicalBoolean) = new FunctionNode("not", b) with ScalarLogicalBoolean

  class ConcatFunction(e: Iterable[ExpressionNode]) extends FunctionNode("concat",e) {
    override def write(sw: StatementWriter) = {
      val s = Session.currentSession
      s.databaseAdapter.writeConcatFunctionCall(this, sw)
    }
  }
  
  // conversions for binary ops like Addition subtraction, multiplication :
  implicit def binaryOpConv1(op: BinaryAMSOp[ByteType,ByteType]) = new NumericalTypeConversion[ByteType](op)
  implicit def binaryOpConv2(op: BinaryAMSOp[ByteType,IntType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv3(op: BinaryAMSOp[ByteType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv4(op: BinaryAMSOp[ByteType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv5(op: BinaryAMSOp[ByteType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv6(op: BinaryAMSOp[ByteType,Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv7(op: BinaryAMSOp[ByteType,Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv8(op: BinaryAMSOp[ByteType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv9(op: BinaryAMSOp[ByteType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv10(op: BinaryAMSOp[ByteType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv11(op: BinaryAMSOp[IntType,ByteType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv12(op: BinaryAMSOp[IntType,IntType]) = new NumericalTypeConversion[IntType](op)
  implicit def binaryOpConv13(op: BinaryAMSOp[IntType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv14(op: BinaryAMSOp[IntType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv15(op: BinaryAMSOp[IntType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv16(op: BinaryAMSOp[IntType,Option[ByteType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv17(op: BinaryAMSOp[IntType,Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv18(op: BinaryAMSOp[IntType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv19(op: BinaryAMSOp[IntType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv20(op: BinaryAMSOp[IntType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv21(op: BinaryAMSOp[LongType,ByteType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv22(op: BinaryAMSOp[LongType,IntType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv23(op: BinaryAMSOp[LongType,LongType]) = new NumericalTypeConversion[LongType](op)
  implicit def binaryOpConv24(op: BinaryAMSOp[LongType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25(op: BinaryAMSOp[LongType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv26(op: BinaryAMSOp[LongType,Option[ByteType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv27(op: BinaryAMSOp[LongType,Option[IntType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv28(op: BinaryAMSOp[LongType,Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv29(op: BinaryAMSOp[LongType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30(op: BinaryAMSOp[LongType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv31(op: BinaryAMSOp[FloatType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv32(op: BinaryAMSOp[FloatType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv33(op: BinaryAMSOp[FloatType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv34(op: BinaryAMSOp[FloatType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv35(op: BinaryAMSOp[FloatType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv36(op: BinaryAMSOp[FloatType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv37(op: BinaryAMSOp[FloatType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv38(op: BinaryAMSOp[FloatType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv39(op: BinaryAMSOp[FloatType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv40(op: BinaryAMSOp[FloatType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv41(op: BinaryAMSOp[DoubleType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv42(op: BinaryAMSOp[DoubleType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv43(op: BinaryAMSOp[DoubleType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv44(op: BinaryAMSOp[DoubleType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45(op: BinaryAMSOp[DoubleType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv46(op: BinaryAMSOp[DoubleType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv47(op: BinaryAMSOp[DoubleType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv48(op: BinaryAMSOp[DoubleType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv49(op: BinaryAMSOp[DoubleType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50(op: BinaryAMSOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv51(op: BinaryAMSOp[Option[ByteType],ByteType]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv52(op: BinaryAMSOp[Option[ByteType],IntType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv53(op: BinaryAMSOp[Option[ByteType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv54(op: BinaryAMSOp[Option[ByteType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv55(op: BinaryAMSOp[Option[ByteType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv56(op: BinaryAMSOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def binaryOpConv57(op: BinaryAMSOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv58(op: BinaryAMSOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv59(op: BinaryAMSOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv60(op: BinaryAMSOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv61(op: BinaryAMSOp[Option[IntType],ByteType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv62(op: BinaryAMSOp[Option[IntType],IntType]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv63(op: BinaryAMSOp[Option[IntType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv64(op: BinaryAMSOp[Option[IntType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv65(op: BinaryAMSOp[Option[IntType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv66(op: BinaryAMSOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv67(op: BinaryAMSOp[Option[IntType],Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def binaryOpConv68(op: BinaryAMSOp[Option[IntType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv69(op: BinaryAMSOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv70(op: BinaryAMSOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv71(op: BinaryAMSOp[Option[LongType],ByteType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv72(op: BinaryAMSOp[Option[LongType],IntType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv73(op: BinaryAMSOp[Option[LongType],LongType]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv74(op: BinaryAMSOp[Option[LongType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75(op: BinaryAMSOp[Option[LongType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv76(op: BinaryAMSOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv77(op: BinaryAMSOp[Option[LongType],Option[IntType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv78(op: BinaryAMSOp[Option[LongType],Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def binaryOpConv79(op: BinaryAMSOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80(op: BinaryAMSOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv81(op: BinaryAMSOp[Option[FloatType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv82(op: BinaryAMSOp[Option[FloatType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv83(op: BinaryAMSOp[Option[FloatType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv84(op: BinaryAMSOp[Option[FloatType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv85(op: BinaryAMSOp[Option[FloatType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv86(op: BinaryAMSOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv87(op: BinaryAMSOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv88(op: BinaryAMSOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv89(op: BinaryAMSOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv90(op: BinaryAMSOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv91(op: BinaryAMSOp[Option[DoubleType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv92(op: BinaryAMSOp[Option[DoubleType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv93(op: BinaryAMSOp[Option[DoubleType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv94(op: BinaryAMSOp[Option[DoubleType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95(op: BinaryAMSOp[Option[DoubleType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv96(op: BinaryAMSOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv97(op: BinaryAMSOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv98(op: BinaryAMSOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv99(op: BinaryAMSOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100(op: BinaryAMSOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  // conversions for binary ops like Division :
  implicit def binaryOpConv1(op: BinaryDivOp[ByteType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv2(op: BinaryDivOp[ByteType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv3(op: BinaryDivOp[ByteType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv4(op: BinaryDivOp[ByteType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv5(op: BinaryDivOp[ByteType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv6(op: BinaryDivOp[ByteType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv7(op: BinaryDivOp[ByteType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv8(op: BinaryDivOp[ByteType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv9(op: BinaryDivOp[ByteType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv10(op: BinaryDivOp[ByteType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv11(op: BinaryDivOp[IntType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv12(op: BinaryDivOp[IntType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv13(op: BinaryDivOp[IntType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv14(op: BinaryDivOp[IntType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv15(op: BinaryDivOp[IntType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv16(op: BinaryDivOp[IntType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv17(op: BinaryDivOp[IntType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv18(op: BinaryDivOp[IntType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv19(op: BinaryDivOp[IntType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv20(op: BinaryDivOp[IntType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv21(op: BinaryDivOp[LongType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv22(op: BinaryDivOp[LongType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv23(op: BinaryDivOp[LongType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv24(op: BinaryDivOp[LongType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv25(op: BinaryDivOp[LongType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv26(op: BinaryDivOp[LongType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv27(op: BinaryDivOp[LongType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv28(op: BinaryDivOp[LongType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv29(op: BinaryDivOp[LongType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv30(op: BinaryDivOp[LongType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv31(op: BinaryDivOp[FloatType,ByteType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv32(op: BinaryDivOp[FloatType,IntType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv33(op: BinaryDivOp[FloatType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv34(op: BinaryDivOp[FloatType,FloatType]) = new NumericalTypeConversion[FloatType](op)
  implicit def binaryOpConv35(op: BinaryDivOp[FloatType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv36(op: BinaryDivOp[FloatType,Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv37(op: BinaryDivOp[FloatType,Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv38(op: BinaryDivOp[FloatType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv39(op: BinaryDivOp[FloatType,Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv40(op: BinaryDivOp[FloatType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv41(op: BinaryDivOp[DoubleType,ByteType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv42(op: BinaryDivOp[DoubleType,IntType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv43(op: BinaryDivOp[DoubleType,LongType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv44(op: BinaryDivOp[DoubleType,FloatType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv45(op: BinaryDivOp[DoubleType,DoubleType]) = new NumericalTypeConversion[DoubleType](op)
  implicit def binaryOpConv46(op: BinaryDivOp[DoubleType,Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv47(op: BinaryDivOp[DoubleType,Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv48(op: BinaryDivOp[DoubleType,Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv49(op: BinaryDivOp[DoubleType,Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv50(op: BinaryDivOp[DoubleType,Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv51(op: BinaryDivOp[Option[ByteType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv52(op: BinaryDivOp[Option[ByteType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv53(op: BinaryDivOp[Option[ByteType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv54(op: BinaryDivOp[Option[ByteType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv55(op: BinaryDivOp[Option[ByteType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv56(op: BinaryDivOp[Option[ByteType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv57(op: BinaryDivOp[Option[ByteType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv58(op: BinaryDivOp[Option[ByteType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv59(op: BinaryDivOp[Option[ByteType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv60(op: BinaryDivOp[Option[ByteType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv61(op: BinaryDivOp[Option[IntType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv62(op: BinaryDivOp[Option[IntType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv63(op: BinaryDivOp[Option[IntType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv64(op: BinaryDivOp[Option[IntType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv65(op: BinaryDivOp[Option[IntType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv66(op: BinaryDivOp[Option[IntType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv67(op: BinaryDivOp[Option[IntType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv68(op: BinaryDivOp[Option[IntType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv69(op: BinaryDivOp[Option[IntType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv70(op: BinaryDivOp[Option[IntType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv71(op: BinaryDivOp[Option[LongType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv72(op: BinaryDivOp[Option[LongType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv73(op: BinaryDivOp[Option[LongType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv74(op: BinaryDivOp[Option[LongType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv75(op: BinaryDivOp[Option[LongType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv76(op: BinaryDivOp[Option[LongType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv77(op: BinaryDivOp[Option[LongType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv78(op: BinaryDivOp[Option[LongType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv79(op: BinaryDivOp[Option[LongType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv80(op: BinaryDivOp[Option[LongType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv81(op: BinaryDivOp[Option[FloatType],ByteType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv82(op: BinaryDivOp[Option[FloatType],IntType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv83(op: BinaryDivOp[Option[FloatType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv84(op: BinaryDivOp[Option[FloatType],FloatType]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv85(op: BinaryDivOp[Option[FloatType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv86(op: BinaryDivOp[Option[FloatType],Option[ByteType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv87(op: BinaryDivOp[Option[FloatType],Option[IntType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv88(op: BinaryDivOp[Option[FloatType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv89(op: BinaryDivOp[Option[FloatType],Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def binaryOpConv90(op: BinaryDivOp[Option[FloatType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv91(op: BinaryDivOp[Option[DoubleType],ByteType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv92(op: BinaryDivOp[Option[DoubleType],IntType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv93(op: BinaryDivOp[Option[DoubleType],LongType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv94(op: BinaryDivOp[Option[DoubleType],FloatType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv95(op: BinaryDivOp[Option[DoubleType],DoubleType]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv96(op: BinaryDivOp[Option[DoubleType],Option[ByteType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv97(op: BinaryDivOp[Option[DoubleType],Option[IntType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv98(op: BinaryDivOp[Option[DoubleType],Option[LongType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv99(op: BinaryDivOp[Option[DoubleType],Option[FloatType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  implicit def binaryOpConv100(op: BinaryDivOp[Option[DoubleType],Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
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
  implicit def unaryOpConv6(op: UnaryAgregateLengthNeutralOp[Option[ByteType]]) = new NumericalTypeConversion[Option[ByteType]](op)
  implicit def unaryOpConv7(op: UnaryAgregateLengthNeutralOp[Option[IntType]]) = new NumericalTypeConversion[Option[IntType]](op)
  implicit def unaryOpConv8(op: UnaryAgregateLengthNeutralOp[Option[LongType]]) = new NumericalTypeConversion[Option[LongType]](op)
  implicit def unaryOpConv9(op: UnaryAgregateLengthNeutralOp[Option[FloatType]]) = new NumericalTypeConversion[Option[FloatType]](op)
  implicit def unaryOpConv10(op: UnaryAgregateLengthNeutralOp[Option[DoubleType]]) = new NumericalTypeConversion[Option[DoubleType]](op)
  
}