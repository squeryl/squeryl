package org.squeryl.tests.schooldb2

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Schema, KeyedEntity}
import org.squeryl.tests.QueryTester
import java.sql.SQLException

trait SchoolDb2Object extends KeyedEntity[Long] {
  val id: Long = 0
}

class Professor(val lastName: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.courseAssignments.left(this)
}

class Course(val subjectId: Long) extends SchoolDb2Object {

  def this() = this(0)

  // Lets support the case where a course can have more than one professor
  lazy val professors = SchoolDb2.courseAssignments.right(this)

  lazy val students = SchoolDb2.courseSubscriptions.left(this)

  lazy val subject = SchoolDb2.subjectToCourses.right(this)
}

class Student(val firstName: String, val lastName: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.courseSubscriptions.right(this)  
}

class Subject(val name: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.subjectToCourses.left(this)
}

class CourseSubscription(val courseId: Int, val studentId: Int, val grade: Float)

class CourseAssignment(val courseId: Long, val professorId: Long)


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

  override def drop = super.drop
}

class SchoolDb2Tests extends QueryTester {

  try {
    SchoolDb2.drop
  }
  catch {
    case e:SQLException => {}
  }

  //loggerOn
  
  SchoolDb2.create

  import SchoolDb2._

  val professeurTournesol = professors.insert(new Professor("Tournesol"))
  val madProfessor = professors.insert(new Professor("Mad Professor"))

  val philosophy = subjects.insert(new Subject("Philosophy"))
  val chemistry = subjects.insert(new Subject("Chemistry"))
  val physics = subjects.insert(new Subject("Physic"))


  val chemistryCourse = courses.insert(new Course(chemistry.id))
  val physicsCourse = courses.insert(new Course(physics.id))


  def testAll = {
    testMetaData
    testMany2ManyAssociationFromLeftSide
    testMany2ManyAssociationsFromRightSide

    testOneToMany
    
    //many2ManyAssociateFromLeft
  }


  def testMetaData = {

    val fmd = courseAssignments.posoMetaData
    
    val professorIdFmd = fmd.findFieldMetaDataForProperty("professorId").getOrElse(error("FieldMetaData for CourseAssignments.professorId not found"))

    passed('testMetaData)
  }

  def testMany2ManyAssociationFromLeftSide = {

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationFromLeftSide)
    
    professeurTournesol.courses.associate(physicsCourse)

    val c1 = professeurTournesol.courses.single : Course

    assertEquals(c1.id,  physicsCourse.id, 'testMany2ManyAssociationFromLeftSide)

    val ca = professeurTournesol.courses.associations.single : CourseAssignment

    assertEquals(ca.courseId,  physicsCourse.id, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(professeurTournesol.courses.dissociateAll, 1, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(professeurTournesol.courses.dissociateAll, 0, 'testMany2ManyAssociationFromLeftSide)

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationFromLeftSide)
    
    passed('testMany2ManyAssociationFromLeftSide)
  }

  def testMany2ManyAssociationsFromRightSide = {

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationsFromRightSide)

    physicsCourse.professors.associate(professeurTournesol)

    val profT = physicsCourse.professors.single : Professor

    assertEquals(professeurTournesol.lastName, profT.lastName, 'testMany2ManyAssociationsFromRightSide)

    val ca = professeurTournesol.courses.associations.single : CourseAssignment

    assertEquals(ca.courseId,  physicsCourse.id, 'testMany2ManyAssociationsFromRightSide)
    
    assertEquals(physicsCourse.professors.dissociateAll, 1, 'testMany2ManyAssociationsFromRightSide)

    assertEquals(physicsCourse.professors.dissociateAll, 0, 'testMany2ManyAssociationsFromRightSide)

    assertEquals(0, courseAssignments.Count : Long, 'testMany2ManyAssociationsFromRightSide)
    
    passed('testMany2ManyAssociationsFromRightSide)
  }
  
  def testOneToMany = {

    val philosophyCourse10AMWednesday = new Course
    val philosophyCourse2PMWednesday = new Course
    val philosophyCourse3PMFriday = new Course

    philosophy.courses.associate(philosophyCourse10AMWednesday)
    philosophy.courses.associate(philosophyCourse2PMWednesday)
    philosophy.courses.associate(philosophyCourse3PMFriday)

    assertEquals(
      philosophy.courses.map(_.id).toSet,
      Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id, philosophyCourse3PMFriday.id),
      'testOneToMany)

    assertEquals(
      philosophyCourse2PMWednesday.subject.single.name,
      philosophy.name,
      'testOneToMany)
    
    passed('testOneToMany)
  }
}