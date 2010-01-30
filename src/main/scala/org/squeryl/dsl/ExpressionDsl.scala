package org.squeryl.dsl

import ast._
import org.squeryl.internals.StatementWriter
import org.squeryl.{Session, Query}

/**
 *  Nemeric types are Byte, Int, Long, Float, Double
 *
 * Rules for Type conversions for +*-/ operations
 *
 * In an expression :
 *
 *   t1:T1 OP t2:T2   : T3
 *
 *  where T1 and T2 are Numeric types, and OP is either +*-/
 *
 *  The resulting type T3 is determined by the following rules :
 *
 * i)   T3 is an Option[] if and only if  either T2 or T2 is an Option[]
 * ii)  T3 is an Agregate if and only if either T2 or T2 is an Agregate
 * iii) T3 is a floating point type (Float or Double) if and only if either T2 or T2 is a floating point type
 *   OR if the OP is a division
 * iv)  T3 has the largest representation (in number of bytes) of T1 and T2
 *      ex.: b1:Byte * l1:Long : Long
 *
 *  (t1:Option[Byte] * t2:Float + l2:Long)  yields a Option[Double]
 *  since
 *   - the largest of the 3 operands (l2:Long) has a representation of 8 bytes,
 *   - there exists a floating point operand (t2:Float)
 *   - there exists an Optio[]
 *
 *  The result must be an Agregate, an Option[] and of 8 bytes,
 *  the type that satisfies all of these constraints is Option[Double]
 *
 *
 *  Scalar                 :  5
 *  ScalarOption           :  5
 *  Agregate               :  2 (AgregateLong and AgregateDouble are the only two non Option agregate)
 *  AgregateOption         :  5
 *
 *  number of numeric types : 17
 *
 *  operators : +/-*        : 4
 *
 *  So each types have 17 signatures for each of the 4 operators, hence 75 arithmetic operators methods each
 *
 */

trait ExpressionDsl {


  type ByteType

  type IntType

  type StringType

  type FloatType

  type DoubleType

  type LongType

  type BooleanType

  type DateType

  class BinaryOperatorNodeScalarByte(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarByte
  class BinaryOperatorNodeScalarByteOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarByteOption
  class BinaryOperatorNodeAgregateByteOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateByteOption
  class BinaryOperatorNodeScalarInt(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarInt
  class BinaryOperatorNodeScalarIntOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarIntOption
  class BinaryOperatorNodeAgregateIntOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateIntOption
  class BinaryOperatorNodeScalarLong(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarLong
  class BinaryOperatorNodeScalarLongOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarLongOption
  class BinaryOperatorNodeAgregateLong(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateLong
  class BinaryOperatorNodeAgregateLongOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateLongOption
  class BinaryOperatorNodeScalarFloat(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarFloat
  class BinaryOperatorNodeScalarFloatOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarFloatOption
  class BinaryOperatorNodeAgregateFloatOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateFloatOption
  class BinaryOperatorNodeScalarDouble(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarDouble
  class BinaryOperatorNodeScalarDoubleOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarDoubleOption
  class BinaryOperatorNodeAgregateDouble(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateDouble
  class BinaryOperatorNodeAgregateDoubleOption(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateDoubleOption
  class BinaryOperatorNodeScalarLogicalBoolean(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with ScalarLogicalBoolean
  class BinaryOperatorNodeAgregateLogicalBoolean(left: ExpressionNode, right: ExpressionNode, op: String) extends BinaryOperatorNode(left,right, op) with AgregateLogicalBoolean

  trait OpArg  {
    
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
  }


  trait ScalarByteASM {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarByte):ScalarByte = new BinaryOperatorNodeScalarByte(this, e, plus)
    def -(e: ScalarByte):ScalarByte = new BinaryOperatorNodeScalarByte(this, e, minus)
    def *(e: ScalarByte):ScalarByte = new BinaryOperatorNodeScalarByte(this, e, times)
  }

  trait ScalarByteOptionASM {
    self: OpArg with ExpressionNode =>
    def +(e: ScalarByteOption):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, times)
  }

  trait ScalarIntASM {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarInt):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, plus)
    def -(e: ScalarInt):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, minus)
    def *(e: ScalarInt):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, times)
  }

  trait ScalarIntOptionASM {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarIntOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarIntOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarIntOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)
  }

  trait ScalarLongASM {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarLong):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, plus)
    def -(e: ScalarLong):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, minus)
    def *(e: ScalarLong):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, times)
  }

  trait ScalarLongOptionASM {
    self: OpArg with ExpressionNode =>
    def +(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
  }

  trait ScalarFloatASMD {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarFloat):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, plus)
    def -(e: ScalarFloat):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, minus)
    def *(e: ScalarFloat):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, times)
    def /(e: ScalarFloat):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)
  }

  trait ScalarFloatOptionASMD {
    self: OpArg with ExpressionNode =>
    def +(e: ScalarFloatOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarFloatOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarFloatOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarFloatOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
  }

  trait ScalarDoubleASMD {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarDouble):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarDouble):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarDouble):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarDouble):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
  }

  trait ScalarDoubleOptionASMD {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarDoubleOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDoubleOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDoubleOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDoubleOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)
  }

  trait AgregateByteOptionASM {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, times)
  }

  trait AgregateIntOptionASM {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
  }

  trait AgregateLongASM { // The only Non Option agregate, used by Select Count(*) ...
    self: OpArg with ExpressionNode =>

    def +(e: AgregateLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, plus)
    def -(e: AgregateLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, minus)
    def *(e: AgregateLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, times)
  }

  trait AgregateDoubleASMD {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateDouble):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: AgregateDouble):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: AgregateDouble):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: AgregateDouble):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)
  }

  trait AgregateLongOptionASM {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
  }

  trait AgregateFloatOptionASMD {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)
  }


  trait AgregateDoubleOptionASMD {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait AgregateDouble2AgregateDoubleOptionASMD {
    self: OpArg with ExpressionNode =>
    def +(e: AgregateDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }
  // Here Integer is in the mathematical sense (non real or rational) it includes Byte,Int,and Long
  trait IntegerToAgregateFloatOptionDivision {
    self: OpArg with ExpressionNode =>

    def /(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)
    def /(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait IntegerToAgregateDoubleOptionDivision {
    self: OpArg with ExpressionNode =>

    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarDouble2AgregateDoubleOptionASMN {
    self: OpArg with ExpressionNode =>
    def +(e: ScalarDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarDouble):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarDoubleOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarLongAndScalarLongOptionCommon {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarByteOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarIntOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarIntOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarIntOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)

    def +(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
  }

  trait AgregatesAbsorbedByDouble2AgregateDoubleOptionASMD {
    self: OpArg with ExpressionNode =>

    def +(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarFloatAndScalarFloatOptionCommon {
    self: OpArg with ExpressionNode =>

    def +(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }


  trait AgregateNumericalOps extends OpArg with ExpressionNode {
    //self: ExpressionNode =>
    //protected def _t: ExpressionNode = this

    def <(e: AgregateNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, lt)
    def >(e: AgregateNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, gt)
    def =?(e: AgregateNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, eq)

    private[squeryl] def lengthInBytes: Int
    private[squeryl] def isOption: Boolean
    private[squeryl] def isAgregate: Boolean
    private[squeryl] def isFloatingPoint: Boolean
  }

  trait ScalarNumerical extends AgregateNumericalOps {
    //self: ExpressionNode =>

    def lessThanz(e: ScalarNumerical): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, lt)
    def <(e: ScalarNumerical): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, lt)
    def >(e: ScalarNumerical): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, gt)
    def =?(e: ScalarNumerical): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, eq)
    def in(e: ListNumerical): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e, in_)
    def between(lower: ScalarNumerical, upper: ScalarNumerical): ScalarLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean

    def ~>(e: ScalarNumerical) = new LeftOuterJoinNode(this, e)
    def <~(e: ScalarNumerical) = new LeftOuterJoinNode(e, this)
    def <~>(e: ScalarNumerical) = new FullOuterJoinNode(this, e)

    def in[T <% ScalarNumerical](e: Query[T]): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e.ast, in_)
  }

  trait AgregateNumerical extends AgregateNumericalOps  {
    self: ExpressionNode =>

    def <(e: ScalarNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, lt)
    def >(e: ScalarNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, gt)
    def =?(e: ScalarNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, eq)

    def in(e: ListNumerical): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, in_)

    def between(lower: ScalarNumerical, upper: ScalarNumerical): AgregateLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean
  }
  
  trait ScalarLogicalBoolean extends OpArg with TypedExpressionNode[Scalar, LogicalBoolean] {
    //self: ExpressionNode  =>
    def _t: ExpressionNode = this
    
    def and(b: ScalarLogicalBoolean) = new BinaryOperatorNodeScalarLogicalBoolean(this, b, _and)
    def or(b: ScalarLogicalBoolean) = new BinaryOperatorNodeScalarLogicalBoolean(this, b, _or)
  }

  trait AgregateLogicalBoolean extends OpArg with ExpressionNode {
    //self: ExpressionNode  =>
    def _t: ExpressionNode = this

    def and(b: AgregateLogicalBoolean) = new BinaryOperatorNodeAgregateLogicalBoolean(this, b, _and)
    def or(b: AgregateLogicalBoolean) = new BinaryOperatorNodeAgregateLogicalBoolean(this, b, _or)
  }


  trait ScalarByte extends OpArg
    with NonLogicalBoolean[Scalar,ByteType]
    with ScalarNumerical
    with ScalarByteASM
    with ScalarByteOptionASM
    with ScalarFloatASMD
    with ScalarFloatOptionASMD
    with ScalarDoubleASMD
    with ScalarDoubleOptionASMD
    with ScalarIntASM
    with ScalarIntOptionASM
    with ScalarLongASM
    with ScalarLongOptionASM
    with AgregateByteOptionASM
    with AgregateIntOptionASM
    with AgregateLongASM
    with AgregateLongOptionASM
    with AgregateFloatOptionASMD
    with AgregateDoubleASMD
    with AgregateDoubleOptionASMD
    with IntegerToAgregateFloatOptionDivision {

    private[squeryl] def lengthInBytes = 1
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def /(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)
    def /(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)
    def /(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    def /(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)


    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarByteOption  extends OpArg
    with NonLogicalBoolean[Scalar,Option[ByteType]]
    with ScalarNumerical
    with ScalarByteOptionASM
    with ScalarFloatOptionASMD
    with ScalarDoubleOptionASMD
    with ScalarIntOptionASM
    with AgregateByteOptionASM
    with AgregateIntOptionASM
    with AgregateLongOptionASM
    with AgregateDoubleOptionASMD
    with IntegerToAgregateFloatOptionDivision
    with AgregateDouble2AgregateDoubleOptionASMD {

    def ~ = this

    private[squeryl] def lengthInBytes = 1
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def +(e: ScalarByte):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, plus)
    def -(e: ScalarByte):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, minus)
    def *(e: ScalarByte):ScalarByteOption = new BinaryOperatorNodeScalarByteOption(this, e, times)
    def /(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def /(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)
    def /(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    //TODO: implement type conversions :
    //def asInt = new DelegateExpression(this) with ScalarInt
  }

  //class DelegateExpression(val expr: Any)

  trait ScalarInt extends OpArg
    with NonLogicalBoolean[Scalar,IntType]
    with ScalarNumerical
    with ScalarIntASM
    with ScalarIntOptionASM
    with ScalarLongASM
    with ScalarFloatASMD
    with ScalarFloatOptionASMD
    with ScalarDoubleASMD
    with ScalarDoubleOptionASMD
    with AgregateLongOptionASM
    with AgregateLongASM
    with AgregateDoubleASMD
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with IntegerToAgregateFloatOptionDivision {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def /(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
    def /(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)
    def /(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarByte):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, plus)
    def -(e: ScalarByte):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, minus)
    def *(e: ScalarByte):ScalarInt = new BinaryOperatorNodeScalarInt(this, e, times)
    def /(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)

    def +(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)
    def /(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)

    def +(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)

    def +(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)
  }

  trait ScalarIntOption  extends OpArg
    with NonLogicalBoolean[Scalar,Option[IntType]]
    with ScalarNumerical
    with ScalarIntOptionASM
    with AgregateIntOptionASM
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with AgregateLongOptionASM
    with ScalarFloatOptionASMD
    with ScalarDoubleOptionASMD
    with ScalarLongOptionASM
    with IntegerToAgregateFloatOptionDivision
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: ScalarByte):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarByte):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarByte):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)


    def +(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarInt):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)


    def /(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarIntOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)
    def /(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)

    def +(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarIntOption = new BinaryOperatorNodeScalarIntOption(this, e, times)
    def /(e: ScalarByteOption):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)

    def +(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarLong extends OpArg
    with NonLogicalBoolean[Scalar,LongType]
    with ScalarNumerical
    with ScalarLongASM
    with ScalarLongOptionASM
    with ScalarDoubleASMD
    with ScalarDoubleOptionASMD
    with AgregateDoubleASMD
    with AgregateLongASM
    with AgregateLongOptionASM
    with AgregateDoubleOptionASMD
    with ScalarLongAndScalarLongOptionCommon
    with IntegerToAgregateDoubleOptionDivision {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: ScalarByte):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, plus)
    def -(e: ScalarByte):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, minus)
    def *(e: ScalarByte):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, times)

    def +(e: ScalarInt):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, plus)
    def -(e: ScalarInt):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, minus)
    def *(e: ScalarInt):ScalarLong = new BinaryOperatorNodeScalarLong(this, e, times)

    def +(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)

    def /(e: ScalarByte):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
    def /(e: ScalarInt):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
    def /(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def /(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

  }


  trait ScalarLongOption  extends OpArg
    with NonLogicalBoolean[Scalar,Option[LongType]]
    with ScalarNumerical
    with ScalarDoubleOptionASMD
    with ScalarLongOptionASM
    with AgregateDoubleOptionASMD
    with AgregateLongOptionASM
    with ScalarLongAndScalarLongOptionCommon
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: ScalarByte):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarByte):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarByte):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarByte):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarInt):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarInt):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarInt):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarInt):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, plus)
    def -(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, minus)
    def *(e: ScalarLong):ScalarLongOption = new BinaryOperatorNodeScalarLongOption(this, e, times)
    def /(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)
  }

  trait ScalarFloat extends OpArg
    with NonLogicalBoolean[Scalar,FloatType]
    with ScalarNumerical
    with ScalarFloatASMD
    with ScalarFloatOptionASMD
    with ScalarDoubleASMD
    with ScalarDoubleOptionASMD
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with ScalarFloatAndScalarFloatOptionCommon
    with AgregateDoubleASMD {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, plus)
    def -(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, minus)
    def *(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, times)
    def /(e: ScalarByte):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)

    def +(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, plus)
    def -(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, minus)
    def *(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, times)
    def /(e: ScalarInt):ScalarFloat = new BinaryOperatorNodeScalarFloat(this, e, div)

    def +(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    def +(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    //

  }

  trait ScalarFloatOption  extends OpArg
    with NonLogicalBoolean[Scalar,Option[FloatType]]
    with ScalarNumerical
    with ScalarFloatOptionASMD
    with ScalarDoubleOptionASMD
    with AgregateDoubleOptionASMD
    with AgregateFloatOptionASMD
    with ScalarFloatAndScalarFloatOptionCommon
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarByte):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarInt):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, plus)
    def -(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, minus)
    def *(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, times)
    def /(e: ScalarFloat):ScalarFloatOption = new BinaryOperatorNodeScalarFloatOption(this, e, div)

    def +(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait ScalarDouble extends OpArg
    with NonLogicalBoolean[Scalar,DoubleType]
    with ScalarNumerical
    with ScalarDoubleASMD
    with ScalarDoubleOptionASMD
    with AgregateDoubleASMD
    with AgregateDoubleOptionASMD
    with AgregatesAbsorbedByDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: ScalarByte):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarByte):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarByte):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarByte):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    def +(e: ScalarInt):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarInt):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarInt):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarInt):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    def +(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarLong):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    def +(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, plus)
    def -(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, minus)
    def *(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, times)
    def /(e: ScalarFloat):ScalarDouble = new BinaryOperatorNodeScalarDouble(this, e, div)

    //

    def +(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)
  }


  trait ScalarDoubleOption extends OpArg
    with NonLogicalBoolean[Scalar,Option[DoubleType]]
    with ScalarNumerical
    with ScalarDoubleOptionASMD
    with AgregateDoubleOptionASMD
    with AgregateDouble2AgregateDoubleOptionASMD
    with AgregatesAbsorbedByDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = false
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: ScalarByte):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarByte):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarByte):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarByte):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarInt):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarInt):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarInt):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarInt):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarLong):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarFloat):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    //

    def +(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarByteOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarIntOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarLongOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, plus)
    def -(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, minus)
    def *(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, times)
    def /(e: ScalarDouble):ScalarDoubleOption = new BinaryOperatorNodeScalarDoubleOption(this, e, div)

  }

  trait AgregateByteOption  extends OpArg
    with NonLogicalBoolean[Agregate,Option[ByteType]]
    with AgregateNumerical
    with AgregateByteOptionASM
    with AgregateIntOptionASM
    with AgregateLongOptionASM
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with IntegerToAgregateFloatOptionDivision
    with ScalarDouble2AgregateDoubleOptionASMN
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 1
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: ScalarByte):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, plus)
    def -(e: ScalarByte):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, minus)
    def *(e: ScalarByte):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, times)
    def /(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateByteOption = new BinaryOperatorNodeAgregateByteOption(this, e, times)
    def /(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    //TODO: implement type conversions :
    //def asInt = new DelegateExpression(this) with AgregateIntOption
  }


  trait AgregateIntOption  extends OpArg
    with NonLogicalBoolean[Agregate,Option[IntType]]
    with AgregateNumerical
    with AgregateIntOptionASM
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with ScalarDouble2AgregateDoubleOptionASMN
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarByte):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarByte):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarByte):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarInt):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateIntOption = new BinaryOperatorNodeAgregateIntOption(this, e, times)
    def /(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def /(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)
  }

  trait AgregateLong extends OpArg
    with NonLogicalBoolean[Agregate,LongType]
    with AgregateNumerical
    with AgregateLongASM
    with AgregateDoubleOptionASMD
    with AgregateDoubleASMD
    with ScalarDouble2AgregateDoubleOptionASMN {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarByte):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, plus)
    def -(e: ScalarByte):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, minus)
    def *(e: ScalarByte):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, times)
    def /(e: ScalarByte):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarInt):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, plus)
    def -(e: ScalarInt):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, minus)
    def *(e: ScalarInt):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, times)
    def /(e: ScalarInt):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, plus)
    def -(e: ScalarLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, minus)
    def *(e: ScalarLong):AgregateLong = new BinaryOperatorNodeAgregateLong(this, e, times)
    def /(e: ScalarLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait AgregateDouble extends OpArg
    with NonLogicalBoolean[Agregate,DoubleType]
    with AgregateNumerical
    with AgregateDoubleASMD
    with AgregateDoubleOptionASMD
    with ScalarDouble2AgregateDoubleOptionASMN {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = false
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarByte):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: ScalarByte):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: ScalarByte):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: ScalarByte):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarInt):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: ScalarInt):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: ScalarInt):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: ScalarInt):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: ScalarLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: ScalarLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: ScalarLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: AgregateLong):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, plus)
    def -(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, minus)
    def *(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, times)
    def /(e: ScalarFloat):AgregateDouble = new BinaryOperatorNodeAgregateDouble(this, e, div)

    def +(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }

  trait AgregateLongOption  extends OpArg
    with NonLogicalBoolean[Agregate,Option[LongType]]
    with AgregateNumerical
    with AgregateDoubleOptionASMD
    with ScalarDouble2AgregateDoubleOptionASMN
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = false

    def ~ = this

    def +(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarByte):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarByte):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarByte):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarByte):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarInt):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarInt):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarInt):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarInt):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLong):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateLongOption = new BinaryOperatorNodeAgregateLongOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }


  trait AgregateFloatOption  extends OpArg
    with NonLogicalBoolean[Agregate,Option[FloatType]]
    with AgregateNumerical
    with AgregateFloatOptionASMD
    with AgregateDoubleOptionASMD
    with ScalarDouble2AgregateDoubleOptionASMN
    with AgregateDouble2AgregateDoubleOptionASMD {

    private[squeryl] def lengthInBytes = 4
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: AgregateIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    //------
    def +(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarByte):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarByteOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarInt):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarIntOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloat):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)

    def +(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateFloatOption = new BinaryOperatorNodeAgregateFloatOption(this, e, div)
  }

  trait AgregateDoubleOption extends OpArg
    with NonLogicalBoolean[Agregate,Option[DoubleType]]
    with AgregateNumerical
    with AgregateDoubleOptionASMD
    with ScalarDouble2AgregateDoubleOptionASMN
    with AgregateDouble2AgregateDoubleOptionASMD{

    private[squeryl] def lengthInBytes = 8
    private[squeryl] def isOption = true
    private[squeryl] def isAgregate = true
    private[squeryl] def isFloatingPoint = true

    def ~ = this

    def +(e: ScalarByte):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarByte):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarByte):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarByte):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarInt):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarInt):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarInt):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarInt):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateByteOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateIntOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLongOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateLong):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: AgregateFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloat):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)

    def +(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, plus)
    def -(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, minus)
    def *(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, times)
    def /(e: ScalarFloatOption):AgregateDoubleOption = new BinaryOperatorNodeAgregateDoubleOption(this, e, div)
  }
  
//  trait AgregateBooleanOperators extends ExpressionNode {
//    //self: ExpressionNode =>
//
//    //def and (right: AgregateBoolean) = new BinaryOperatorNode(this, right,"and",true) with AgregateBoolean
//    //def or  (right: AgregateBoolean) = new BinaryOperatorNode(this, right,"or")  with AgregateBoolean
//  }

  trait AgregateBooleanOption extends TypedExpressionNode[Agregate,Option[BooleanType]]
  
  //trait AgregateBoolean extends TypedExpressionNode[Agregate,BooleanType]

  trait ScalarBoolean extends TypedExpressionNode[Scalar,BooleanType]

  trait ScalarBooleanOption extends TypedExpressionNode[Scalar,Option[BooleanType]] 

  trait NonLogicalBoolean[K <: ExpressionKind, T] extends TypedExpressionNode[K,T]  



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

  trait ScalarString extends BaseScalarString with NonLogicalBoolean[Scalar,StringType] {
    def ~ = this
    def ||(e: ScalarString)  = new ConcatFunction(List(this,e)) with ScalarString
  }
  
  trait ScalarStringOption extends BaseScalarString with NonLogicalBoolean[Scalar,Option[StringType]] {
    def ~ = this
    def ||(e: ScalarString)  = new ConcatFunction(List(this,e)) with ScalarStringOption
  }
  
  trait AgregateStringOption extends NonLogicalBoolean[Agregate,Option[StringType]] {

    def ||(e: ScalarString)  = new ConcatFunction(List(this,e)) with AgregateStringOption
    def in(e: ListString) = new BinaryOperatorNodeAgregateLogicalBoolean(this,e,"in")
  }

  trait DateAbsorbantOps extends ExpressionNode with OpArg {
    def <(e: AgregateDateOption): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, lt)
    def >(e: AgregateDateOption): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, gt)
    def =?(e: AgregateDateOption): AgregateLogicalBoolean = new BinaryOperatorNodeAgregateLogicalBoolean(this, e, eq)
    //def in(e: Query[AgregateDateOption]): AgregateLogicalBoolean= new BinaryOperatorNodeAgregateLogicalBoolean(this, e.ast, in_)
    def between(lower: AgregateDateOption, upper: AgregateDateOption): ScalarLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean    
  }

  trait BaseScalarDate extends DateAbsorbantOps {

    def <(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, lt)
    def >(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, gt)
    def =?(e: BaseScalarDate): ScalarLogicalBoolean = new BinaryOperatorNodeScalarLogicalBoolean(this, e, eq)
    def in[S <% BaseScalarDate](e: Query[S]): ScalarLogicalBoolean= new BinaryOperatorNodeScalarLogicalBoolean(this, e.ast, in_)
    def between(lower: BaseScalarDate, upper: BaseScalarDate): ScalarLogicalBoolean = error("implement me") //new BinaryOperatorNode(this, lower, div) with ScalarLogicalBoolean
  }

  trait ScalarDate extends BaseScalarDate with NonLogicalBoolean[Scalar,DateType] {
    def ~ = this
  }

  trait ScalarDateOption extends BaseScalarDate with NonLogicalBoolean[Scalar,Option[DateType]] {
    def ~ = this
  }

  trait AgregateDateOption extends NonLogicalBoolean[Agregate,Option[DateType]] {
    def in(e: ListDate) = new BinaryOperatorNodeAgregateLogicalBoolean(this,e,"in")
  }

  def not(b: ScalarLogicalBoolean) = new FunctionNode("not", b) with ScalarLogicalBoolean

  class ConcatFunction(e: Iterable[ExpressionNode]) extends FunctionNode("concat",e) {
    override def write(sw: StatementWriter) = {
      val s = Session.currentSession
      s.databaseAdapter.writeConcatFunctionCall(this, sw)
    }
  }
}
