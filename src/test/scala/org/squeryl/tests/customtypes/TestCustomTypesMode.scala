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
package org.squeryl.tests.customtypes


import org.squeryl.tests.QueryTester
import java.sql.SQLException
import org.squeryl.customtypes._
import org.squeryl.{KeyedEntity, Schema}

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
    validateQuery('simpleSelect1, patients.where(_.age > 70), (p:Patient)=>p.id.value, List(joseCuervo.id.value))

    drop
  }
}

class HospitalDb extends Schema {

  val patients = table[Patient]
  
  drop
  create

  override def drop = super.drop
}

class Patient(var firstName: FirstName, var age: Option[Age], var weight: Option[WeightInKilograms]) extends KeyedEntity[IntField] {

  def this() = this(null, Some(new Age(1)),Some(new WeightInKilograms(1)))

  var id: IntField = null
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
