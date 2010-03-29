package org.squeryl.tests.schooldb2

import org.squeryl.dsl.ast.EqualityExpression

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Query, Schema, Table, KeyedEntity}
import org.squeryl.dsl.DelegateQuery
import org.squeryl.tests.QueryTester
import java.sql.SQLException

trait SchoolDb2Object extends KeyedEntity[Long] {
  val id: Long = 0
}

class Professor(val lastName: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.courseAssignments.left(this)
}

class Course(val subjectId: Long) extends SchoolDb2Object {

  lazy val professor = SchoolDb2.courseAssignments.right(this)
}

class Subject(val name: String) extends SchoolDb2Object

class CourseSubscription(val courseId: Int, val studentId: Int, val grade: Float)

class CourseAssignment(val courseId: Long, val professorId: Long)


object SchoolDb2 extends Schema {

  val professors = table[Professor]

  val courses = table[Course]

  val subjects = table[Subject]

  val courseAssignments =
    manyToMany(professors, courses).
    via[CourseAssignment]((p,c,a) => (p.id === a.professorId, a.courseId === c.id))

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

  val tournesol = professors.insert(new Professor("Tournesol"))
  val madProfessor = professors.insert(new Professor("Mad Professor"))

  val physics = subjects.insert(new Subject("Physic"))
  
  val physicsCourse = courses.insert(new Course(physics.id))


  def testAll = {
    testMetaData
    many2ManyAssociateFromLeft
  }


  def testMetaData = {

    val fmd = courseAssignments.posoMetaData
    
    val professorIdFmd = fmd.findFieldMetaDataForProperty("professorId").getOrElse(error("FieldMetaData for CourseAssignments.professorId not found"))

    println('testMetaData + " passed")
  }

  def many2ManyAssociateFromLeft = {

    tournesol.courses.associate(physicsCourse)

    val c = tournesol.courses.single

    assertEquals(c.id,  physicsCourse.id, 'many2ManyAssociateFromLeft)

    //tournesol.courses.dissociateAll
    
    passed('many2ManyAssociateFromLeft)
  }
}