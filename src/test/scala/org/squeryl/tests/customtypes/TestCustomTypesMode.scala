package org.squeryl.tests.customtypes


import org.squeryl.tests.QueryTester
import org.squeryl.{Schema}
import java.sql.SQLException
import org.squeryl.customtypes._

class TestCustomTypesMode extends QueryTester {

  val hospitalDb = new HospitalDb

  import hospitalDb._

  val testObjects = new {
    
    val joseCuervo = patients.insert(new Patient(new FirstName("Jose"), Some(new Age(76)), Some(new WeightInKilograms(290.134))))
    val raoulEspinoza = patients.insert(new Patient(new FirstName("Raoul"), Some(new Age(32)), None))
  }

  import testObjects._
  import CustomTypesMode._

  def simpleSelect =
    from(patients)(p =>
      where(p.age > 70)
      select(p)
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

/**
 * En example of trait that can be added to custom types,
 * to add meta data and validation 
 */
trait Domain[A] {
  self: CustomType =>

  def label: String
  def validate(a: A): Unit
  def value: A

  validate(value)
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