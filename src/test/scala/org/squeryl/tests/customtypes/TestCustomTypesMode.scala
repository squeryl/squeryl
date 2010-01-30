package org.squeryl.tests.customtypes

import org.squeryl.dsl.ast.SelectElementReference
import org.squeryl.internals.FieldReferenceLinker
import org.squeryl.tests.QueryTester
import org.squeryl.{CustomType, Schema, CustomTypesMode}
import java.sql.SQLException
import java.util.Date


class TestCustomTypesMode extends QueryTester {

  val hospitalDb = new HospitalDb

  import hospitalDb._

  val testObjects = new {
    
    val joseCuervo = patients.insert(new Patient(new FirstName("Jose"), Some(new Age(76)), Some(new WeightInKilograms(290.134))))
    val raoulEspinoza = patients.insert(new Patient(new FirstName("Raoul"), Some(new Age(32)), None))
  }

  import testObjects._
  import MyCustomTypesMode._

  def simpleSelect =
    From(patients)(p =>
    ~:Where(p.age > 70)
      Select(p)
    )

  def testAll = {

    validateQuery('simpleSelect, simpleSelect, (p:Patient)=>p.id.value, List(joseCuervo.id.value))
  }
}

class HospitalDb extends Schema {

  val patients = table[Patient]

  try {
    drop // we normally *NEVER* do this !!
  }
  catch {
    case e:SQLException => println(" schema does not yet exist :" + e.getMessage)
  }

  create
}

class Patient(var firstName: FirstName, var age: Option[Age], var weight: Option[WeightInKilograms]) {

  def this() = this(null, Some(new Age(1)),Some(new WeightInKilograms(1)))

  var id: IntField = null
}


object MyCustomTypesMode extends CustomTypesMode {

  type IntType = IntField

  type StringType = StringField

  type DoubleType = DoubleField

  type FloatType = FloatField

  type LongType = LongField

  type BooleanType = BooleanField

  type DateType = DateField

  protected def mapInt2IntType(i: Int) = new IntField(i)
  protected def mapString2StringType(s: String) = new StringField(s)
  protected def mapDouble2DoubleType(d: Double) = new DoubleField(d)
  protected def mapFloat2FloatType(d: Float) = new FloatField(d)
  protected def mapLong2LongType(l: Long) = new LongField(l)
  protected def mapBoolean2BooleanType(b: Boolean) = new BooleanField(b)
  protected def mapDate2DateType(b: Date) = new DateField(b)

  protected val sampleInt: IntType = new IntField(0)
  protected val sampleString: StringType = new StringField("")
  protected val sampleDouble: DoubleType = new DoubleField(0.0)
  protected val sampleFloat: FloatType = new FloatField(0.0F)
  protected def sampleLong = new LongField(1)
  protected def sampleBoolean = new BooleanField(false)
  protected def sampleDate = new DateField(new Date)

  def createLeafNodeOfScalarIntType(i: IntField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with ScalarInt
  def createLeafNodeOfScalarIntOptionType(i: Option[IntField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with ScalarIntOption

  def createLeafNodeOfScalarStringType(s: StringField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with ScalarString
  def createLeafNodeOfScalarStringOptionType(s: Option[StringField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with ScalarStringOption

  def createLeafNodeOfScalarDoubleType(i: DoubleField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarDouble
  def createLeafNodeOfScalarDoubleOptionType(i: Option[DoubleField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarDoubleOption

  def createLeafNodeOfScalarFloatType(i: FloatField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarFloat
  def createLeafNodeOfScalarFloatOptionType(i: Option[FloatField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarFloatOption

  def createLeafNodeOfScalarLongType(i: LongField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarLong
  def createLeafNodeOfScalarLongOptionType(l: Option[LongType]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarLongOption

  def createLeafNodeOfScalarBooleanType(i: BooleanField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarBoolean

  def createLeafNodeOfScalarBooleanOptionType(i: Option[BooleanField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarBooleanOption


  def createLeafNodeOfScalarDateType(i: DateField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarDate

  def createLeafNodeOfScalarDateOptionType(i: Option[DateField]) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with  ScalarDateOption

  def createLeafNodeOfAgregateIntType(i: IntField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateIntOption

  def createLeafNodeOfAgregateStringType(s: StringField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateStringOption

  def createLeafNodeOfAgregateDoubleType(i: DoubleField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateDoubleOption

  def createLeafNodeOfAgregateFloatType(i: FloatField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateFloatOption

  def createLeafNodeOfAgregateLongType(i: LongField) =
    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateLongOption

//  def createLeafNodeOfAgregateBooleanType(i: BooleanField) =
//    new SelectElementReference(FieldReferenceLinker.takeLastAccessedFieldReference.get) with AgregateBoolean
}

trait Domain[A] {
  self: CustomType =>
  
  def label: String
  def validate(a: A): Unit
  def value: A
    
  validate(value)
}

class IntField(val value: Int) extends CustomType {
  def wrappedValue: Any = value
}

class StringField(val value: String) extends CustomType {
  def wrappedValue: Any = value
}

class DoubleField(val value: Double) extends CustomType {
  def wrappedValue: Any = value
}

class FloatField(val value: Float) extends CustomType {
  def wrappedValue: Any = value
}

class LongField(val value: Long) extends CustomType {
  def wrappedValue: Any = value
}

class BooleanField(val value: Boolean) extends CustomType {
  def wrappedValue: Any = value
}

class DateField(val value: Date) extends CustomType {
  def wrappedValue: Any = value
}

class Age(v: Int) extends IntField(v) with Domain[Int] {
  def validate(a: Int) = assert(a > 0, "age must be positive, got " + a)
  def label = "age"
}

class FirstName(v: String) extends StringField(v) with Domain[String] {
  def validate(s: String) = assert(s.length <= 50, "first name is waaaay to long : " + s)
  def label = "first name"
}

class WeightInKilograms(v: Double) extends DoubleField(v) with Domain[Double] {
  def validate(d:Double) = assert(d > 0, "weight must be positive, got " + d) 
  def label = "weight (in kilograms)"
}

class ReasonOfVisit(v: String) extends StringField(v) with Domain[String] {
  def validate(s:String) = assert(s.length > 1, "invalid visit reason : " + s)
  def label = "reason of visit"
}