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
package org.squeryl.tests

import org.squeryl.Schema


object Inheritance extends Schema with QueryTester {


  class Person(var name: String, var lastName: String, var age: Option[Int]) {
    def this() = this(null,null,Some(0))
    var id: Int = 0
  }

  trait Employee {
    self: Person =>

    var personId: Int = 0
    var hourlySalary: Double = 0.0
  }

  val e:Employee = new Person() with Employee

  val hs = e.hourlySalary
}
