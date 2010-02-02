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

  def createLeafNodeOfScalarIntType(i: Int) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Int](i) with ScalarInt
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with ScalarInt
    }

  def createLeafNodeOfScalarIntOptionType(i: Option[IntType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Int]](i) with ScalarIntOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with ScalarIntOption
    }

  def createLeafNodeOfScalarStringType(s: String) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[String](s, true) with ScalarString
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with ScalarString
    }
  }

  def createLeafNodeOfScalarStringOptionType(s: Option[StringType]) = {
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[String]](s, true) with ScalarStringOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with ScalarStringOption
    }
  }

  def createLeafNodeOfScalarDoubleType(i: Double) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Double](i) with ScalarDouble
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarDouble
    }

  def createLeafNodeOfScalarDoubleOptionType(i: Option[Double]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Double]](i) with ScalarDoubleOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarDoubleOption
    }


  def createLeafNodeOfScalarFloatType(i: Float) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Float](i) with ScalarFloat
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarFloat
    }

  def createLeafNodeOfScalarFloatOptionType(i: Option[Float]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Float]](i) with ScalarFloatOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarFloatOption
    }

  def createLeafNodeOfScalarLongType(i: Long) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Long](i) with ScalarLong
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarLong
    }

  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Long]](l) with ScalarLongOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with ScalarLongOption
    }

  def createLeafNodeOfScalarBooleanType(i: Boolean) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Boolean](i) with ScalarBoolean
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarBoolean
    }

  def createLeafNodeOfScalarBooleanOptionType(b: Option[BooleanType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Boolean]](b) with ScalarBooleanOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarBooleanOption
    }

  def createLeafNodeOfScalarDateType(i: Date) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Date](i) with ScalarDate
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarDate
    }

  def createLeafNodeOfScalarDateOptionType(b: Option[DateType]) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Option[Date]](b) with ScalarDateOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with  ScalarDateOption
    }


  def createLeafNodeOfAgregateIntType(i: Int) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Int](i) with AgregateIntOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with AgregateIntOption
    }

  def createLeafNodeOfAgregateStringType(s: String) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[String](s, true) with AgregateStringOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with AgregateStringOption
    }

  def createLeafNodeOfAgregateDoubleType(i: Double) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Double](i) with AgregateDoubleOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with AgregateDoubleOption
    }

  def createLeafNodeOfAgregateFloatType(i: Float) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Float](i) with AgregateFloatOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with AgregateFloatOption
    }

  def createLeafNodeOfAgregateLongType(i: Long) =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantExpressionNode[Long](i) with AgregateLongOption
      case Some(n:SelectElement) =>
        new SelectElementReference(n) with AgregateLongOption
    }

//  def createLeafNodeOfAgregateBooleanType(i: Boolean) =
//    FieldReferenceLinker.takeLastAccessedFieldReference match {
//      case None =>
//        new ConstantExpressionNode[Boolean](i) with AgregateBoolean
//      case Some(n:SelectElement) =>
//        new SelectElementReference(n) with AgregateBoolean
//    }
//
//  def createLeafNodeOfAgregateDateType(i: Date) =
//    FieldReferenceLinker.takeLastAccessedFieldReference match {
//      case None =>
//        new ConstantExpressionNode[Date](i) with AgregateDate
//      case Some(n:SelectElement) =>
//        new SelectElementReference(n) with AgregateDate
//    }

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