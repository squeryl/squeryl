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