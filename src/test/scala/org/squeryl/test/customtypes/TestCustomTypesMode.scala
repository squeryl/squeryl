package org.squeryl.test.customtypes

/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
import java.sql.SQLException
import org.squeryl.{KeyedEntity, Schema}
import org.squeryl.framework._
import org.squeryl.customtypes._

import CustomTypesMode._

 
abstract class TestCustomTypesMode extends SchemaTester with QueryTester with RunTestsInsideTransaction {

  val schema = new HospitalDb

  import schema._

  var sharedTestObjects : TestData = null

  override def prePopulate(){
    sharedTestObjects = new TestData(schema)
  }


  import CustomTypesMode._

  def simpleSelect =
    from(patients)(p =>
      where(p.age > 70)
      select(p)
    )

  test("Queries"){
    val testObjects = sharedTestObjects; import testObjects._

    validateQuery('simpleSelect, simpleSelect, (p:Patient)=>p.id.value, List(joseCuervo.id.value))
    validateQuery('simpleSelect1, patients.where(_.age > 70), (p:Patient)=>p.id.value, List(joseCuervo.id.value))
    validateQuery('simpleSelect2, patients.where(_.age < 40), (p:Patient)=>p.id.value, List(raoulEspinoza.id.value))
    validateQuery('simpleSelect3, patients.where(_.age < Some(new Age(40))), (p:Patient)=>p.id.value, List(raoulEspinoza.id.value))
  }

  test("OneToMany"){
    val testObjects = sharedTestObjects; import testObjects._

    val jose = patients.where(_.age > 70).single
    val pi = new PatientInfo(new Info("!!!!!"))

    val pi0 = new PatientInfo(new Info("zzzz"))

    jose.patientInfo.assign(pi0)
    assert(jose.id.value == pi0.patientId.value)
    patientInfo.insert(pi0)        

    jose.patientInfo.associate(pi)

  }
}

class TestData(schema : HospitalDb){
  val joseCuervo = schema.patients.insert(new Patient(new FirstName("Jose"), Some(new Age(76)), Some(new WeightInKilograms(290.134))))
  val raoulEspinoza = schema.patients.insert(new Patient(new FirstName("Raoul"), Some(new Age(32)), None))
}

object HospitalDb extends HospitalDb

class HospitalDb extends Schema {
  
  val patients = table[Patient]

  val patientInfo = table[PatientInfo]

  val patienttoPatientInfo =
      oneToManyRelation(patients, patientInfo).
      via((p,pi) => p.id === pi.patientId)
  
  override def drop = super.drop
}

class Patient(var firstName: FirstName, var age: Option[Age], var weight: Option[WeightInKilograms]) extends KeyedEntity[IntField] {

  def this() = this(null, Some(new Age(1)),Some(new WeightInKilograms(1)))

  var id: IntField = null

  lazy val patientInfo = HospitalDb.patienttoPatientInfo.left(this)
}

class PatientInfo(val info: Info) extends KeyedEntity[IntField] {

  def this() = this(new Info(""))
  
  val patientId: IntField = null

  val id: IntField = null

  lazy val patient = HospitalDb.patienttoPatientInfo.right(this)
}

/**
 * En example of trait that can be added to custom types,
 * to add meta data and validation 
 */
trait Domain[A] {
  self: Product1[Any] =>

  def label: String
  def validate(a: A): Unit
  def value: A

  validate(value)
}

class Age(v: Int) extends IntField(v) with Domain[Int] {
  // secondary constructor to show  #93
  def this(s: String) = this(s.toInt)
  
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

class Info(v: String) extends StringField(v) with Domain[String] {
  def validate(s:String) = {}
  def label = "info"
}


