package org.squeryl.test

import org.squeryl.dsl.CompositeKey2
import org.squeryl.framework.{DBConnector, DbTestBase}
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl.{KeyedEntity, Schema}

import scala.util.{Success, Failure, Try}

case class Manager(val name: String) extends KeyedEntity[Long] {
  val id: Long = 0

  lazy val employees = IncludeSchema.manager_employee_relation.leftIncludable(this)
  lazy val responsibilities = IncludeSchema.manager_responsibility_relation.leftIncludable(this)
}

case class Employee(val name: String, val managerId: Long) extends KeyedEntity[Long] {
  val id: Long = 0

  lazy val benefits = IncludeSchema.employee_benefit_relation.leftIncludable(this)
  lazy val manager = IncludeSchema.manager_employee_relation.rightIncludable(this)
}

case class Responsibility(val name: String, val managerId: Long) extends KeyedEntity[Long] {
  val id: Long = 0

  lazy val types = IncludeSchema.responsibility_responsibilityType_relation.leftIncludable(this)
}

case class ResponsibilityType(val name: String, val responsibilityId: Long, val managerId: Long) extends KeyedEntity[CompositeKey2[Long, Long]] {
  def id = compositeKey(responsibilityId, managerId)

  lazy val responsibility = IncludeSchema.responsibility_responsibilityType_relation.rightIncludable(this)
}

case class Benefit(val name: String, val employeeId: Long) extends KeyedEntity[Long] {
  val id: Long = 0

  lazy val categories = IncludeSchema.benefit_category_relation.leftIncludable(this)
  lazy val expenses = IncludeSchema.benefit_expense_relation.leftIncludable(this)
  lazy val employee = IncludeSchema.employee_benefit_relation.rightIncludable(this)
}

case class Category(val name: String, val benefitId: Long) extends KeyedEntity[Long] {
  val id: Long = 0
}

case class Expense(val name: String, val benefitId: Long) extends KeyedEntity[Long] {
  val id: Long = 0
}

object IncludeSchema extends Schema {
  val managers = table[Manager]
  val employees = table[Employee]
  val responsibilities = table[Responsibility]
  val responsibilityTypes = table[ResponsibilityType]
  val benefits = table[Benefit]
  val categories = table[Category]
  val expenses = table[Expense]

  val manager_employee_relation = oneToManyRelation(managers, employees).via((m, e) => m.id === e.managerId)
  val manager_responsibility_relation = oneToManyRelation(managers, responsibilities).via((m, r) => m.id === r.managerId)
  val responsibility_responsibilityType_relation = oneToManyRelation(responsibilities, responsibilityTypes).via((r, t) => r.id === t.responsibilityId)
  val employee_benefit_relation = oneToManyRelation(employees, benefits).via((e, b) => e.id === b.employeeId)
  val benefit_category_relation = oneToManyRelation(benefits, categories).via((b, c) => b.id === c.benefitId)
  val benefit_expense_relation = oneToManyRelation(benefits, expenses).via((b, e) => b.id === e.benefitId)

  def reset() = {
    drop // its protected for some reason
    create
  }
}

abstract class IncludeTest extends DbTestBase {
  self: DBConnector =>

  // Single Include tests

  test("include oneToMany - many relation with one child") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head
    }

    assert(data.employees.size == 1)
  }

  test("include oneToMany - many relation with two children") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c1 = IncludeSchema.employees.insert(new Employee("child1", p.id))
      val c2 = IncludeSchema.employees.insert(new Employee("child2", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head
    }

    assert(data.employees.size == 2)
  }

  test("include oneToMany - many relation with two parents and two children") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p1 = IncludeSchema.managers.insert(new Manager("person1"))
      val p2 = IncludeSchema.managers.insert(new Manager("person2"))
      val c1 = IncludeSchema.employees.insert(new Employee("child1", p1.id))
      val c2 = IncludeSchema.employees.insert(new Employee("child2", p2.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).toList
    }

    assert(data.size == 2)
    assert(data.filter(p => p.name == "person1").head.employees.head.name == "child1")
    assert(data.filter(p => p.name == "person2").head.employees.head.name == "child2")
  }

  test("include oneToMany - many relation with no data returns empty") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).toList
    }

    assert(data.size == 0)
  }

  test("include oneToMany - can iterate included property multiple times") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head
    }

    assert(data.employees.size == 1)
    assert(data.employees.size == 1)
  }

  test("include oneToMany - many relation with no children returns empty") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head
    }

    assert(data.employees.size == 0)
  }

  test("include oneToMany - can delete children") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      val data = from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head

      data.employees.deleteAll

      assert(data.employees.size == 0)
    }
  }

  test("include oneToMany - can associate children") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = new Employee("child", p.id)

      val data = from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees))).head

      data.employees.associate(c)

      assert(data.employees.size == 1)

      data.employees.refresh

      assert(data.employees.size == 1)
    }
  }

  test("include oneToMany - with no data returns empty size") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m1 = IncludeSchema.managers.insert(new Manager("manager1"))

      from(IncludeSchema.managers)(p => select(p) include(_-*(_.employees))).head
    }

    assert(data.employees.size == 0)
  }

//   end Single Include tests

  // begin Nested Include tests

  test("include oneToMany - can retrieve two adjacent properties") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("person"))
      val e = IncludeSchema.employees.insert(new Employee("child", m.id))
      val r = IncludeSchema.responsibilities.insert(new Responsibility("responsibility", m.id))

      from(IncludeSchema.managers)(p => select(p)
      include(_.->>(_.-*(_.employees), _.-*(_.responsibilities)))).head
    }

    assert(data.employees.size == 1)
    assert(data.responsibilities.size == 1)
  }

  test("include oneToMany - can retrieve two nested properties") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("person"))
      val e = IncludeSchema.employees.insert(new Employee("child", m.id))
      val r = IncludeSchema.benefits.insert(new Benefit("benefit", e.id))

      from(IncludeSchema.managers)(p => select(p)
      include(_.-*(_.employees).-*(_.benefits))).head
    }

    assert(data.employees.size == 1)
    assert(data.employees.head.benefits.size == 1)
  }

  test("include oneToMany - can retrieve two nested properties with correct assignments") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("manager"))
      val e1 = IncludeSchema.employees.insert(new Employee("employee1", m.id))
      val b1 = IncludeSchema.benefits.insert(new Benefit("benefit1", e1.id))
      val e2 = IncludeSchema.employees.insert(new Employee("employee2", m.id))
      val b2 = IncludeSchema.benefits.insert(new Benefit("benefit2", e2.id))

      from(IncludeSchema.managers)(p => select(p)
      include(_.-*(_.employees).-*(_.benefits))).head
    }

    assert(data.employees.size == 2)
    assert(data.employees.head.benefits.size == 1)
    assert(data.employees.filter(_.name == "employee1").head.benefits.head.name == "benefit1")
    assert(data.employees.last.benefits.size == 1)
    assert(data.employees.filter(_.name == "employee2").head.benefits.head.name == "benefit2")
  }

  test("include oneToMany - can retrieve nested properties on adjacent properties with correct assignments") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("person"))
      val e = IncludeSchema.employees.insert(new Employee("employee", m.id))
      val b = IncludeSchema.benefits.insert(new Benefit("benefit", e.id))
      val r = IncludeSchema.responsibilities.insert(new Responsibility("responsibility", m.id))
      val rt = IncludeSchema.responsibilityTypes.insert(new ResponsibilityType("responsibilityType", r.id, m.id))

      from(IncludeSchema.managers)(p => select(p)
      include(_.->>(_.-*(_.employees).-*(_.benefits), _.-*(_.responsibilities).-*(_.types)))).head
    }

    assert(data.employees.size == 1)
    assert(data.employees.head.benefits.size == 1)
    assert(data.responsibilities.size == 1)
    assert(data.responsibilities.head.types.size == 1)
  }

  test("include oneToMany - can retrieve nested properties with adjacent properties") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("person"))
      val e = IncludeSchema.employees.insert(new Employee("employee", m.id))
      val b = IncludeSchema.benefits.insert(new Benefit("benefit", e.id))
      val r = IncludeSchema.categories.insert(new Category("category", b.id))
      val ex = IncludeSchema.expenses.insert(new Expense("expense", b.id))

      from(IncludeSchema.managers)(p => select(p)
      include(_.->>(_.-*(_.employees).-*(_.benefits).->>(
                                                          _.-*(_.categories), _.-*(_.expenses)),
                    _.-*(_.responsibilities).-*(_.types)))).head
    }

    assert(data.employees.size == 1)
    assert(data.employees.head.name == "employee")
    assert(data.employees.head.benefits.size == 1)
    assert(data.employees.head.benefits.head.name == "benefit")
    assert(data.employees.head.benefits.head.categories.size == 1)
    assert(data.employees.head.benefits.head.categories.head.name == "category")
    assert(data.employees.head.benefits.head.expenses.size == 1)
    assert(data.employees.head.benefits.head.expenses.head.name == "expense")
  }

  // end Nested Include tests

  // many to one tests

  test("include manyToOne - one relation") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("manager"))
      val c = IncludeSchema.employees.insert(new Employee("employee", p.id))

      from(IncludeSchema.employees)(p => select(p) include(_.*-(_.manager))).head
    }

    assert(data.manager.one.nonEmpty)
    assert(data.manager.one.get.name == "manager")
  }

  test("include manyToOne - oneToMany to manyToOne nested") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("manager"))
      val e = IncludeSchema.employees.insert(new Employee("employee", m.id))
      val h = IncludeSchema.benefits.insert(new Benefit("benefit", e.id))

      from(IncludeSchema.employees)(p => select(p) include(_.*-(_.manager).-*(_.responsibilities).-*(_.types).*-(_.responsibility))).toList
    }
  }

  test("include oneToMany - returns distinct of each entity with where clause when reincluding same type") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m = IncludeSchema.managers.insert(new Manager("manager"))
      val e1 = IncludeSchema.employees.insert(new Employee("employee1", m.id))
      val e2 = IncludeSchema.employees.insert(new Employee("employee2", m.id))

      from(IncludeSchema.employees)(p =>
        where(p.id === e1.id)
        select(p) include(_.*-(_.manager).-*(_.employees))).toList
    }

    assert(data.length == 1)
  }

  test("include all different relations") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val m2 = IncludeSchema.managers.insert(new Manager("badManager"))
      val r2 = IncludeSchema.responsibilities.insert(new Responsibility("badResponsibility", m2.id))
      val rt2 = IncludeSchema.responsibilityTypes.insert(new ResponsibilityType("badResponsibilityType", r2.id, m2.id))

      val m1 = IncludeSchema.managers.insert(new Manager("manager1"))
      val r = IncludeSchema.responsibilities.insert(new Responsibility("responsibility1", m1.id))
      val rt = IncludeSchema.responsibilityTypes.insert(new ResponsibilityType("responsibilityType1", r.id, m1.id))

      val e1 = IncludeSchema.employees.insert(new Employee("employee1", m1.id))
      val b1 = IncludeSchema.benefits.insert(new Benefit("benefit1", e1.id))
      val ex = IncludeSchema.expenses.insert(new Expense("expense1", b1.id))

      val e2 = IncludeSchema.employees.insert(new Employee("employee2", m1.id))
      val b2 = IncludeSchema.benefits.insert(new Benefit("benefit2", e2.id))
      val c2 = IncludeSchema.categories.insert(new Category("category2", b2.id))


      from(IncludeSchema.employees)(p => select(p) include(_->>(
        _.*-(_.manager).-*(_.responsibilities).-*(_.types),
        _.-*(_.benefits).->>(
                              _.-*(_.expenses), _.-*(_.categories))))).toList.filter(_.name == "employee1").head
    }

    assert(data.benefits.size == 1)
    assert(data.benefits.head.name == "benefit1")
    assert(data.benefits.head.expenses.size == 1)
    assert(data.benefits.head.expenses.head.name == "expense1")
    assert(data.benefits.head.categories.size == 0)

    assert(data.manager.one.nonEmpty)
    assert(data.manager.one.get.name == "manager1")
    assert(data.manager.one.get.responsibilities.size == 1)
    assert(data.manager.one.get.responsibilities.head.name == "responsibility1")
    assert(data.manager.one.get.responsibilities.head.types.size == 1)
    assert(data.manager.one.get.responsibilities.head.types.head.name == "responsibilityType1")
  }

  // end many to one tests

  // begin inhibitWhen tests

  test("inhibitWhen - direct descendant") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees).inhibitWhen(true))).head
    }

    assert(Try(data.employees.size) match {
      case Success(_) => false
      case Failure(_:java.lang.IllegalStateException) => true
    }, "Expected IllegalStateException when accessing inhibited child property")
  }

  test("inhibitWhen - nested descendant") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.-*(_.employees).-*(_.benefits).inhibitWhen(true))).head
    }

    assert(data.employees.size == 1)
    assert(Try(data.employees.head.benefits.size) match {
      case Success(_) => false
      case Failure(_:java.lang.IllegalStateException) => true
    }, "Expected IllegalStateException when accessing inhibited child property")
  }

  test("inhibitWhen - adjacent descendants") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.->>(_.-*(_.employees), _.-*(_.responsibilities)).inhibitWhen(true))).head
    }

    assert(Try(data.employees.size) match {
      case Success(_) => false
      case Failure(_:java.lang.IllegalStateException) => true
    }, "Expected IllegalStateException when accessing inhibited child property")

    assert(Try(data.responsibilities.size) match {
      case Success(_) => false
      case Failure(_:java.lang.IllegalStateException) => true
    }, "Expected IllegalStateException when accessing inhibited child property")
  }

  test("inhibitWhen - adjacent descendants inner inhibited") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      from(IncludeSchema.managers)(p => select(p) include(_.->>(_.-*(_.employees).inhibitWhen(true), _.-*(_.responsibilities).inhibitWhen(false)))).head
    }

    assert(Try(data.employees.size) match {
      case Success(_) => false
      case Failure(_:java.lang.IllegalStateException) => true
    }, "Expected IllegalStateException when accessing inhibited child property")

    assert(data.responsibilities.size == 0)
  }

  // end inhibitWhen tests

  test("join") {
    implicit val schema = IncludeSchema
    transaction {
      IncludeSchema.reset
    }

    val data = transaction {
      val p = IncludeSchema.managers.insert(new Manager("person"))
      val c = IncludeSchema.employees.insert(new Employee("child", p.id))

      join(IncludeSchema.managers, IncludeSchema.employees)((m, e) =>
        select(m)
        include(_.-*(_.responsibilities))
        on(m.id === e.managerId)
      ).head
    }

    assert(data.responsibilities.size == 0)
  }
}

