package org.squeryl.tests.mutablerelations

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.tests.QueryTester
import org.squeryl._
import dsl.CompositeKey2

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
    via[CourseAssignment]((p,c,a) => (p.id === a.professorId, a.courseId === c.id))

  val courseSubscriptions =
    manyToManyRelation(courses, students).
    via[CourseSubscription]((c,s,cs) => (cs.studentId === s.id, c.id === cs.courseId))

  val subjectToCourses =
    oneToManyRelation(subjects, courses).
    via((s,c) => s.id === c.subjectId)

  // the default constraint for all foreign keys in this schema :
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) =
    foreignKeyDeclaration.constrainReference

  override def drop = super.drop
}

class SchoolDb2MetableRelations extends QueryTester {

  SchoolDb2.drop

  //loggerOn

  SchoolDb2.create

  import SchoolDb2._

  val professeurTournesol = professors.insert(new Professor("Tournesol"))
  val madProfessor = professors.insert(new Professor("Mad Professor"))

  val philosophy = subjects.insert(new Subject("Philosophy"))
  val chemistry = subjects.insert(new Subject("Chemistry"))
  val physics = subjects.insert(new Subject("Physic"))
  val computationTheory = subjects.insert(new Subject("Computation Theory"))


  val chemistryCourse = courses.insert(new Course(chemistry.id))
  val physicsCourse = courses.insert(new Course(physics.id))


  def testAll = {

    //loggerOn
    
    testMany2ManyAssociationFromLeftSide
    testMany2ManyAssociationsFromRightSide

    testOneToMany    

    SchoolDb2.drop
  }


  def testMany2ManyAssociationFromLeftSide = {

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationFromLeftSide)

    professeurTournesol.courses.associate(physicsCourse)

    val c1 = professeurTournesol.courses.head : Course

    assertEquals(c1.id,  physicsCourse.id, 'testMany2ManyAssociationFromLeftSide)

    val ca = professeurTournesol.courses.associations.head : CourseAssignment

    assertEquals(ca.courseId,  physicsCourse.id, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(professeurTournesol.courses.dissociateAll, 1, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(professeurTournesol.courses.dissociateAll, 0, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationFromLeftSide)

    passed('testMany2ManyAssociationFromLeftSide)
  }

  def testMany2ManyAssociationsFromRightSide = {

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationsFromRightSide)

    physicsCourse.professors.associate(professeurTournesol)

    val profT = physicsCourse.professors.head : Professor

    assertEquals(professeurTournesol.lastName, profT.lastName, 'testMany2ManyAssociationsFromRightSide)

    professeurTournesol.courses.refresh

    val ca = professeurTournesol.courses.associations.head : CourseAssignment

    assertEquals(ca.courseId,  physicsCourse.id, 'testMany2ManyAssociationsFromRightSide)

    assertEquals(physicsCourse.professors.dissociateAll, 1, 'testMany2ManyAssociationsFromRightSide)

    assertEquals(physicsCourse.professors.dissociateAll, 0, 'testMany2ManyAssociationsFromRightSide)

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationsFromRightSide)

    // test dissociate :
    physicsCourse.professors.associate(professeurTournesol)

    physicsCourse.professors.head : Professor

    professeurTournesol.courses.refresh

    assertEquals(physicsCourse.professors.dissociate(professeurTournesol), true, 'testMany2ManyAssociationsFromRightSide)
    assertEquals(physicsCourse.professors.dissociate(professeurTournesol), false, 'testMany2ManyAssociationsFromRightSide)

    passed('testMany2ManyAssociationsFromRightSide)
  }

  def testOneToMany = {

    val philosophyCourse10AMWednesday = new Course
    val philosophyCourse2PMWednesday = new Course
    val philosophyCourse3PMFriday = new Course

    val c1 = philosophy.courses.associate(philosophyCourse10AMWednesday)
    val c2 = philosophy.courses.associate(philosophyCourse2PMWednesday)
    val c3 = philosophy.courses.associate(philosophyCourse3PMFriday)

    val c4 = chemistry.courses.associate(new Course)


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
        assertEquals(3, sCnt, 'testOneToMany)
      else if(s0.id == chemistry.id)
        assertEquals(2, sCnt, 'testOneToMany)
      else
        error("unknown subject : " + s0)
    }

    assertEquals(5, cnt, 'testOneToMany)

    assertEquals(
      philosophy.courses.map(_.id).toSet,
      Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id, philosophyCourse3PMFriday.id),
      'testOneToMany)

    // no need to refresh :
    //philosophyCourse2PMWednesday.subject.refresh
    // since the relation is lazy and we haven't touched it yet...

    assertEquals(
      philosophyCourse2PMWednesday.subject.one.get.name,
      philosophy.name,
      'testOneToMany)

    // verify that a reassociation does an update and not an insert :
    val pk1 = philosophyCourse3PMFriday.id

    computationTheory.courses.associate(philosophyCourse3PMFriday)

    assertEquals(
      pk1,
      philosophyCourse3PMFriday.id,
      'testOneToMany)

    philosophy.courses.refresh
    
    // verify that the reassociation worked, which means that
    // 1) : the set of philosophy.courses was reduced properly
    assertEquals(
      philosophy.courses.map(_.id).toSet,
      Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id),
      'testOneToMany)

    // 2) philosophyCourse3PMFriday.subject points to the proper subject
    assertEquals(
      computationTheory.name,
      philosophyCourse3PMFriday.subject.one.get.name,
      'testOneToMany)

    passed('testOneToMany)
  }
}
