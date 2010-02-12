package org.squeryl


import dsl.ast._
import dsl.QueryDsl
import internals.FieldReferenceLinker
import java.util.Date

/**
 *  This factory is meant to use POSOs (Plain old Scala Objects),
 * i.e. your object use Scala's primitive types to map to columns.
 * This can have a significantl performance advantage over using object
 * types i.e. a result set of N rows of objects with M field will generate
 * N * M objects for the garbage collector, while POSOs with primitive types
 * will each count for 1 for the garbage collector (to be more precise,
 * String and Option[] fields will add a +1 in both cases, but a custom String wrapper will
 * allso add one ref, for a total of 2 refs vs a single ref per string column
 * for the POSO).
 *  This lightweight strategy has a cost : constants and object field references
 * cannot distinguish at compile time, so this mode is less 'strict' than
 * one with a CustomType 
 */
object PrimitiveTypeMode extends QueryDsl {
  
  type IntType = Int

  type StringType = String

  type DoubleType = Double

  type FloatType = Float

  type LongType = Long

  type BooleanType = Boolean

  type DateType = Date

  //TODO: consider spliting createLeafNodeOfScalarIntType in two factory methods : createConstantOfXXXType and createReferenceOfXXXType 

  def createLeafNodeOfScalarIntType(i: IntType) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Int](i) with NumericalExpression[IntType]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with NumericalExpression[IntType]
    }

  def createLeafNodeOfScalarIntOptionType(i: Option[IntType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Int]](i) with NumericalExpression[Option[IntType]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with NumericalExpression[Option[Int]]
    }

  def createLeafNodeOfScalarStringType(s: String) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[String](s, true) with StringExpression[String]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with StringExpression[String]
    }
  }

  def createLeafNodeOfScalarStringOptionType(s: Option[StringType]) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[String]](s, true) with StringExpression[Option[String]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with StringExpression[Option[String]]
    }
  }

  def createLeafNodeOfScalarDoubleType(i: Double) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Double](i) with NumericalExpression[Double]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  NumericalExpression[Double]
    }

  def createLeafNodeOfScalarDoubleOptionType(i: Option[Double]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Double]](i) with NumericalExpression[Option[Double]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  NumericalExpression[Option[Double]]
    }


  def createLeafNodeOfScalarFloatType(i: Float) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Float](i) with NumericalExpression[Float]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  NumericalExpression[Float]
    }

  def createLeafNodeOfScalarFloatOptionType(i: Option[Float]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Float]](i) with NumericalExpression[Option[Float]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  NumericalExpression[Option[Float]]
    }

  def createLeafNodeOfScalarLongType(i: Long) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Long](i) with NumericalExpression[Long]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  NumericalExpression[Long]
    }

  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Long]](l) with NumericalExpression[Option[Long]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with NumericalExpression[Option[Long]]
    }

  def createLeafNodeOfScalarBooleanType(i: Boolean) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Boolean](i) with BooleanExpression[Boolean]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  BooleanExpression[Boolean]
    }

  def createLeafNodeOfScalarBooleanOptionType(b: Option[BooleanType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Boolean]](b) with BooleanExpression[Option[Boolean]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  BooleanExpression[Option[Boolean]]
    }

  def createLeafNodeOfScalarDateType(i: Date) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Date](new java.sql.Date(i.getTime)) with DateExpression[Date]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  DateExpression[Date]
    }

  def createLeafNodeOfScalarDateOptionType(b: Option[DateType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Date]](b) with DateExpression[Option[Date]]
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  DateExpression[Option[Date]]
    }



  protected def mapInt2IntType(i: Int) = i
  protected def mapString2StringType(s: String) = s
  protected def mapDouble2DoubleType(d: Double) = d
  protected def mapFloat2FloatType(d: Float) = d
  protected def mapLong2LongType(l: Long) = l
  protected def mapBoolean2BooleanType(b: Boolean) = b
  protected def mapDate2DateType(b: Date) = b

  protected val sampleInt: IntType = 0
  protected val sampleString: StringType = ""
  protected val sampleDouble: DoubleType = 0.0
  protected val sampleFloat: FloatType = 0.0F
  protected val sampleLong: LongType = 0
  protected val sampleBoolean: BooleanType = false
  protected val sampleDate: DateType = new Date
}