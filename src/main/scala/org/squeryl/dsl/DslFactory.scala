package org.squeryl.dsl

import ast._
import org.squeryl.internals.{OutMapper}
import java.sql.ResultSet
import java.util.Date


trait DslFactory
  extends TypeArithmetic
    with SqlFunctions {

  protected def createLeafNodeOfScalarIntType(i: IntType): NumericalExpression[IntType]
  protected def createLeafNodeOfScalarIntOptionType(i: Option[IntType]): NumericalExpression[Option[IntType]]

  protected def createLeafNodeOfScalarDoubleType(d: DoubleType): NumericalExpression[DoubleType]
  protected def createLeafNodeOfScalarDoubleOptionType(d: Option[DoubleType]): NumericalExpression[Option[DoubleType]]

  protected def createLeafNodeOfScalarFloatType(d: FloatType): NumericalExpression[FloatType]
  protected def createLeafNodeOfScalarFloatOptionType(d: Option[FloatType]): NumericalExpression[Option[FloatType]]

  protected def createLeafNodeOfScalarStringType(s: StringType): StringExpression[StringType]
  protected def createLeafNodeOfScalarStringOptionType(s: Option[StringType]): StringExpression[Option[StringType]]

  protected def createLeafNodeOfScalarLongType(s: LongType): NumericalExpression[LongType]
  protected def createLeafNodeOfScalarLongOptionType(s: Option[LongType]): NumericalExpression[Option[LongType]]

  protected def createLeafNodeOfScalarBooleanType(s: BooleanType): BooleanExpression[BooleanType]
  protected def createLeafNodeOfScalarBooleanOptionType(s: Option[BooleanType]): BooleanExpression[Option[BooleanType]]

  protected def createLeafNodeOfScalarDateType(d: DateType): DateExpression[DateType]
  protected def createLeafNodeOfScalarDateOptionType(d: Option[DateType]): DateExpression[Option[DateType]]

  // expose Factory Methods implicit :
  // ScalarNode Types :
  implicit def int2ScalarInt(i: IntType) = createLeafNodeOfScalarIntType(i)

  implicit def double2ScalarDouble(d: DoubleType) = createLeafNodeOfScalarDoubleType(d)

  implicit def float2ScalarFloat(d: FloatType) = createLeafNodeOfScalarFloatType(d)

  implicit def string2ScalarString(s: StringType) = createLeafNodeOfScalarStringType(s)

  implicit def long2ScalarLong(l: LongType) = createLeafNodeOfScalarLongType(l)

  implicit def bool2ScalarBoolean(b: BooleanType) = createLeafNodeOfScalarBooleanType(b)

  implicit def date2ScalarDate(b: DateType) = createLeafNodeOfScalarDateType(b)

  implicit def optionInt2ScalarInt(i: Option[IntType]) = createLeafNodeOfScalarIntOptionType(i)

  implicit def optionLong2ScalarLong(i: Option[LongType]) = createLeafNodeOfScalarLongOptionType(i)

  implicit def optionString2ScalarString(i: Option[StringType]) = createLeafNodeOfScalarStringOptionType(i)

  implicit def optionDouble2ScalarDouble(i: Option[DoubleType]) = createLeafNodeOfScalarDoubleOptionType(i)

  implicit def optionFloat2ScalarFloat(i: Option[FloatType]) = createLeafNodeOfScalarFloatOptionType(i)

  implicit def optionBoolean2ScalarBoolean(i: Option[BooleanType]) = createLeafNodeOfScalarBooleanOptionType(i)

  implicit def optionDate2ScalarDate(i: Option[DateType]) = createLeafNodeOfScalarDateOptionType(i)

  
  // List Conversion implicits don't vary with the choice of
  // column/field types, so they don't need to be overridable factory methods :
  implicit def listOfInt2ListInt(l: List[IntType]) =
    new ConstantExpressionNodeList[IntType](l) with ListInt

  //TODO: relax type equivalence using Numerical, since databases allow things like 1 in (0.4,0.4, ...)
  implicit def listOfDouble2ListDouble(l: List[DoubleType]) =
    new ConstantExpressionNodeList[DoubleType](l) with ListDouble

  implicit def listOfFloat2ListFloat(l: List[FloatType]) =
    new ConstantExpressionNodeList[FloatType](l) with ListFloat

  implicit def listOfLong2ListLong(l: List[LongType]) =
    new ConstantExpressionNodeList[LongType](l) with ListLong

  implicit def listOfString2ListString(l: List[StringType]) =
    new ConstantExpressionNodeList[StringType](l) with ListString

  implicit def listOfDate2ListDate(l: List[DateType]) =
    new ConstantExpressionNodeList[DateType](l) with ListDate

  implicit def typedExpression2OrderByArg[E <% TypedExpressionNode[_]](e: E) = new OrderByArg(e)

  implicit def orderByArg2OrderByExpression(a: OrderByArg) = new OrderByExpression(a)

  protected def mapInt2IntType(i: Int): IntType
  protected def mapString2StringType(s: String): StringType
  protected def mapDouble2DoubleType(d: Double): DoubleType
  protected def mapFloat2FloatType(d: Float): FloatType
  protected def mapLong2LongType(l: Long): LongType
  protected def mapBoolean2BooleanType(b: Boolean): BooleanType
  protected def mapDate2DateType(b: Date): DateType
  
  //TODO: make these methods protected when COMPILER won't crash !!!
  def createOutMapperIntType: OutMapper[IntType] = new OutMapper[IntType] {
    def doMap(rs: ResultSet) = mapInt2IntType(rs.getInt(index))
    def sample = sampleInt
  }
  
  def createOutMapperStringType: OutMapper[StringType] = new OutMapper[StringType] {
    def doMap(rs: ResultSet) = mapString2StringType(rs.getString(index))
    def sample = sampleString
  }

  def createOutMapperDoubleType: OutMapper[DoubleType] = new OutMapper[DoubleType] {
    def doMap(rs: ResultSet) = mapDouble2DoubleType(rs.getDouble(index))
    def sample = sampleDouble
  }

  def createOutMapperFloatType: OutMapper[FloatType] = new OutMapper[FloatType] {
    def doMap(rs: ResultSet) = mapFloat2FloatType(rs.getFloat(index))
    def sample = sampleFloat
  }

  def createOutMapperLongType: OutMapper[LongType] = new OutMapper[LongType] {
    def doMap(rs: ResultSet) = mapLong2LongType(rs.getLong(index))
    def sample = sampleLong
  }

  def createOutMapperBooleanType: OutMapper[BooleanType] = new OutMapper[BooleanType] {
    def doMap(rs: ResultSet) = mapBoolean2BooleanType(rs.getBoolean(index))
    def sample = sampleBoolean
  }

  def createOutMapperDateType: OutMapper[DateType] = new OutMapper[DateType] {
    def doMap(rs: ResultSet) = mapDate2DateType(rs.getDate(index))
    def sample = sampleDate
  }

  def createOutMapperIntTypeOption: OutMapper[Option[IntType]] = new OutMapper[Option[IntType]] {
    def doMap(rs: ResultSet) = {
      val v = mapInt2IntType(rs.getInt(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleInt)
  }

  def createOutMapperDoubleTypeOption: OutMapper[Option[DoubleType]] = new OutMapper[Option[DoubleType]] {
    def doMap(rs: ResultSet) = {
      val v = mapDouble2DoubleType(rs.getDouble(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleDouble)
  }

  def createOutMapperFloatTypeOption: OutMapper[Option[FloatType]] = new OutMapper[Option[FloatType]] {
    def doMap(rs: ResultSet) = {
      val v = mapFloat2FloatType(rs.getFloat(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleFloat)
  }

  def createOutMapperStringTypeOption: OutMapper[Option[StringType]] = new OutMapper[Option[StringType]] {
    def doMap(rs: ResultSet) = {
      val v = mapString2StringType(rs.getString(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleString)
  }

  def createOutMapperLongTypeOption: OutMapper[Option[LongType]] = new OutMapper[Option[LongType]] {
    def doMap(rs: ResultSet) = {
      val v = mapLong2LongType(rs.getLong(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleLong)
  }

  def createOutMapperBooleanTypeOption: OutMapper[Option[BooleanType]] = new OutMapper[Option[BooleanType]] {
    def doMap(rs: ResultSet) = {
      val v = mapBoolean2BooleanType(rs.getBoolean(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleBoolean)
  }

  def createOutMapperDateTypeOption: OutMapper[Option[DateType]] = new OutMapper[Option[DateType]] {
    def doMap(rs: ResultSet) = {
      val v = mapDate2DateType(rs.getDate(index))
      if(rs.wasNull)
        None
      else
        Some(v)
    }
    def sample = Some(sampleDate)
  }
}
