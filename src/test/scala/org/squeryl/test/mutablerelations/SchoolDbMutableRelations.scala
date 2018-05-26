package org.squeryl.test.mutablerelations

import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl._
import org.squeryl.dsl.CompositeKey2

trait SchoolDb2Object extends KeyedEntity[Long] {
  val id: Long = 0
}

class Professor(val lastName: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.courseAssignments.leftStateful(this)
}

class Course(val subjectId: Long) extends SchoolDb2Object {

  def this() = this(0)

  // Lets support the case where a course can have more than one professor
  lazy val professors = SchoolDb2.courseAssignments.rightStateful(this)

  lazy val students = SchoolDb2.courseSubscriptions.leftStateful(this)

  lazy val subject = SchoolDb2.subjectToCourses.rightStateful(this)
}

class Student(val firstName: String, val lastName: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.courseSubscriptions.rightStateful(this)
}

class Subject(val name: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.subjectToCourses.leftStateful(this)
}

class CourseSubscription(val courseId: Long, val studentId: Long, val grade: Float) extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(courseId, studentId)
}

class CourseAssignment(val courseId: Long, val professorId: Long) extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(courseId, professorId)
}


object SchoolDb2 extends Schema {

  val professors = table[Professor]

  val students = table[Student]

  val courses = table[Course]

  val subjects = table[Subject]

  val courseAssignments =
    manyToManyRelation(professors, courses).
    via[CourseAssignment]((p,c,a) => (a.professorId === p.id, a.courseId === c.id))

  val courseSubscriptions =
    manyToManyRelation(courses, students).
    via[CourseSubscription]((c,s,cs) => (cs.studentId === s.id, c.id === cs.courseId))

  val subjectToCourses =
    oneToManyRelation(subjects, courses).
    via((s,c) => c.subjectId === s.id)

  // the default constraint for all foreign keys in this schema :
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) =
    foreignKeyDeclaration.constrainReference

  override def drop = {
    Session.cleanupResources
    super.drop
  }
}

import org.squeryl.framework._

abstract class SchoolDb2MetableRelations extends SchemaTester with QueryTester with RunTestsInsideTransaction {
  self: DBConnector =>

  val schema = SchoolDb2

  def instance = new {

    import schema._

    val professeurTournesol = professors.insert(new Professor("Tournesol"))
    val madProfessor = professors.insert(new Professor("Mad Professor"))

    val philosophy = subjects.insert(new Subject("Philosophy"))
    val chemistry = subjects.insert(new Subject("Chemistry"))
    val physics = subjects.insert(new Subject("Physic"))
    val computationTheory = subjects.insert(new Subject("Computation Theory"))


    val chemistryCourse = courses.insert(new Course(chemistry.id))
    val physicsCourse = courses.insert(new Course(physics.id))
  }


  test("Many2ManyAssociationFromLeftSide"){

    import SchoolDb2._

    val i = instance
    import i._

    courseAssignments.Count.toLong shouldBe 0

    professeurTournesol.courses.associate(physicsCourse)

    val c1 = professeurTournesol.courses.head : Course

    c1.id shouldBe  physicsCourse.id

    val ca = professeurTournesol.courses.associations.head : CourseAssignment

    ca.courseId shouldBe  physicsCourse.id

    professeurTournesol.courses.dissociateAll shouldBe 1

    professeurTournesol.courses.dissociateAll shouldBe 0

    courseAssignments.Count.toLong shouldBe 0

    passed('testMany2ManyAssociationFromLeftSide)
  }

  test("Many2ManyAssociationFromRightSide"){

    import SchoolDb2._
    val i = instance
    import i._

    courseAssignments.Count.toLong shouldBe 0

    physicsCourse.professors.associate(professeurTournesol)

    val profT = physicsCourse.professors.head : Professor

    professeurTournesol.lastName shouldBe profT.lastName

    professeurTournesol.courses.refresh

    val ca = professeurTournesol.courses.associations.head : CourseAssignment

    ca.courseId shouldBe  physicsCourse.id

    physicsCourse.professors.dissociateAll shouldBe 1

    physicsCourse.professors.dissociateAll shouldBe 0

    courseAssignments.Count.toLong shouldBe 0

    // test dissociate :
    physicsCourse.professors.associate(professeurTournesol)

    physicsCourse.professors.head : Professor

    professeurTournesol.courses.refresh

    physicsCourse.professors.dissociate(professeurTournesol) shouldBe true
    physicsCourse.professors.dissociate(professeurTournesol) shouldBe false

    passed('testMany2ManyAssociationsFromRightSide)
  }

  test("OneToMany"){

    import SchoolDb2._
    val i = instance
    import i._

    val philosophyCourse10AMWednesday = new Course
    val philosophyCourse2PMWednesday = new Course
    val philosophyCourse3PMFriday = new Course

    philosophy.courses.associate(philosophyCourse10AMWednesday)
    philosophy.courses.associate(philosophyCourse2PMWednesday)
    philosophy.courses.associate(philosophyCourse3PMFriday)

    chemistry.courses.associate(new Course)


    val s = from(subjects)(s0 =>
      where(s0.id notIn(Seq(computationTheory.id, physics.id)))
      select(s0)
    )

    var cnt = 0

    for(s0 <- s ) {
      var sCnt = 0
      for(c <- s0.courses) {
        cnt += 1
        sCnt += 1
      }
      if(s0.id == philosophy.id)
        3 shouldBe sCnt
      else if(s0.id == chemistry.id)
        2 shouldBe sCnt
      else
        org.squeryl.internals.Utils.throwError("unknown subject : " + s0)
    }

    5 shouldBe cnt

    philosophy.courses.map(_.id).toSet shouldBe Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id, philosophyCourse3PMFriday.id)

    // no need to refresh :
    //philosophyCourse2PMWednesday.subject.refresh
    // since the relation is lazy and we haven't touched it yet...
    philosophyCourse2PMWednesday.subject.one.get.name shouldBe philosophy.name

    // verify that a reassociation does an update and not an insert :
    val pk1 = philosophyCourse3PMFriday.id

    computationTheory.courses.associate(philosophyCourse3PMFriday)
    pk1 shouldBe philosophyCourse3PMFriday.id

    philosophy.courses.refresh

    // verify that the reassociation worked, which means that
    // 1) : the set of philosophy.courses was reduced properly
    philosophy.courses.map(_.id).toSet shouldBe Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id)

    // 2) philosophyCourse3PMFriday.subject points to the proper subject
    computationTheory.name shouldBe philosophyCourse3PMFriday.subject.one.get.name

    passed('testOneToMany)
  }
}
