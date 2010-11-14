package org.squeryl.tests.schooldb2

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.tests.QueryTester
import org.squeryl._
import dsl.ast._
import dsl.{OneToMany, CompositeKey2}
import java.sql.{Savepoint}

trait SchoolDb2Object extends KeyedEntity[Long] {
  val id: Long = 0
}

object SchoolDb2 extends SchoolDb2

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

  def fullName = compositeKey(firstName, lastName)
}

class Subject(val name: String) extends SchoolDb2Object {

  lazy val courses = SchoolDb2.subjectToCourses.left(this)
}

class CourseSubscription(val courseId: Long, val studentId: Long, var grade: Float) extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(courseId, studentId)
}

class CourseAssignment(val courseId: Long, val professorId: Long) extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(courseId, professorId)
}

case class Entry(text: String) extends KeyedEntity[Int] {
 val id:Int = 0
 // entryToComments is a one-to-many relation:
 lazy val comments: OneToMany[Comment] = SchoolDb2.entryToComments.left(this)
}

case class Comment(text: String, entryId: Int = 0, userId: Int = 0)
   extends KeyedEntity[Int] {
 val id:Int = 0
}



class ASTConstructionInterferenceA extends KeyedEntity[Long] {
  val id: Long = 0

  lazy val bs = SchoolDb2.aToB.left(this)
}

class ASTConstructionInterferenceB(val aId: Long) extends KeyedEntity[Long] {
  val id: Long = 0

  val field1 = "abc"
  val field2 = field1
}



class SchoolDb2 extends Schema {

  val entries = table[Entry]
  val comments = table[Comment]("commentz")

  val entryToComments = oneToManyRelation(entries, comments).via(
    (e,c) => e.id === c.entryId)
  
  val professors = table[Professor]

  val students = table[Student]


  on(students)(s => declare(
    s.firstName is(indexed),
    s.lastName defaultsTo("!"),
    s.fullName is(unique, indexed),
    columns(s.id, s.firstName, s.lastName) are(indexed)  
  ))

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

  //now we will redefine some of the foreign key constraints :
  //if we delete a subject, we want all courses to be deleted
  subjectToCourses.foreignKeyDeclaration.constrainReference(onDelete cascade)

  //when a course is deleted, all of the subscriptions will get deleted :
  courseSubscriptions.leftForeignKeyDeclaration.constrainReference(onDelete cascade)

  override def drop = super.drop

  val as = table[ASTConstructionInterferenceA]
  val bs = table[ASTConstructionInterferenceB]

  val aToB =
    oneToManyRelation(as, bs).
    via((a, b) => a.id === b.aId)

  aToB.foreignKeyDeclaration.unConstrainReference
}

class SchoolDb2Tests extends QueryTester {

  val schema = new SchoolDb2
  
  import schema._

  schema.drop

  //loggerOn

  schema.create

  lazy val seedData = new {
    
    val professeurTournesol = professors.insert(new Professor("Tournesol"))
    val madProfessor = professors.insert(new Professor("Mad Professor"))

    val philosophy = subjects.insert(new Subject("Philosophy"))
    val chemistry = subjects.insert(new Subject("Chemistry"))
    val physics = subjects.insert(new Subject("Physic"))
    val computationTheory = subjects.insert(new Subject("Computation Theory"))


    val chemistryCourse = courses.insert(new Course(chemistry.id))
    val physicsCourse = courses.insert(new Course(physics.id))

    val xiaoJimbao = students.insert(new Student("Xiao", "Jimbao"))
  }

  def dumpSchema =
     SchoolDb2.printDdl

  def testAll = {

    testIssue68

    val entry = entries.insert(Entry("An entry"))
    val comment = Comment("A single comment")
    entry.comments.associate(comment)

    from(entry.comments)(c => where(c.id === comment.id) select(c))
    
    seedData

    testUpdateWithCompositePK
    
    testCompositeEquality

    testMany2ManyAssociationFromLeftSide
    testMany2ManyAssociationsFromRightSide

    testOneToMany

    testUniquenessConstraint
    
    SchoolDb2.drop
  }

  def testUpdateWithCompositePK = {
    import seedData._

    val xiao = students.lookup(xiaoJimbao.id).get

    val courseSubscription = xiao.courses.assign(chemistryCourse)

    courseSubscriptions.insert(courseSubscription)
    courseSubscription.grade = 95.0F
    courseSubscriptions.update(courseSubscription)

    val cs2 = courseSubscriptions.lookup(courseSubscription.id).get
    
    assertEquals(95.0F, cs2.grade, 'testUpdateWithCompositePK)

    passed('testUpdateWithCompositePK)
  }

  def testMany2ManyAssociationFromLeftSide = {

    import seedData._

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

    import seedData._

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

    import seedData._

    val pc = philosophy.id

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

    // verify that a reassociation does an update and not an insert :
    val pk1 = philosophyCourse3PMFriday.id

    computationTheory.courses.associate(philosophyCourse3PMFriday)

    assertEquals(
      pk1,
      philosophyCourse3PMFriday.id,
      'testOneToMany)

    // verify that the reassociation worked, which means that
    // 1) : the set of philosophy.courses was reduced properly
    assertEquals(
      philosophy.courses.map(_.id).toSet,
      Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id),
      'testOneToMany)

    // 2) philosophyCourse3PMFriday.subject points to the proper subject
    assertEquals(
      computationTheory.name,
      philosophyCourse3PMFriday.subject.single.name,
      'testOneToMany)

    passed('testOneToMany)
  }

  def testCompositeEquality = {

    import seedData._
    
    val a = physicsCourse.professors.associate(professeurTournesol)

    val qA = courseAssignments.lookup(compositeKey(a.courseId, a.professorId))

    _existsAndEquals(qA, a)

    val qA2 =
      from(courseAssignments)(ca =>
        where(ca.id ===(a.courseId, a.professorId))
        select(ca)
      )

    _existsAndEquals(qA2.headOption, a)

    //println(qA2.statement)

    val qA3 =
      courseAssignments.where(_.id === a.id)

    _existsAndEquals(qA3.headOption, a)

    courseAssignments.delete(compositeKey(a.courseId, a.professorId))

    assertEquals(0L, qA3.Count: Long, 'testCompositeEquality)

    //println(ca2.statement)
    passed('testCompositeEquality)
  }

  private def _existsAndEquals(oca: Option[CourseAssignment], ca: CourseAssignment) = {

    if(oca == None)
      error("query returned no rows")

    assertEquals(ca.id, oca.get.id, 'testCompositeEquality)
  }

  def testUniquenessConstraint = {

    import seedData._
    
    assertEquals(0, courseAssignments.Count : Long, 'testUniquenessConstraint)

    physicsCourse.professors.associate(professeurTournesol)

    assertEquals(1, courseAssignments.Count : Long, 'testUniquenessConstraint)

    var exceptionThrown = false

    val s = Session.currentSession

    val sp: Option[Savepoint] =
      if(s.databaseAdapter.failureOfStatementRequiresRollback)
        Some(s.connection.setSavepoint)
      else
        None
    
    try {
      physicsCourse.professors.associate(professeurTournesol)
    }
    catch {
      case e:RuntimeException => {
        exceptionThrown = true
        sp.foreach(s.connection.rollback(_))
      }
    }

    if(! exceptionThrown)
      error('testUniquenessConstraint + " failed, unique constraint violation occured")

    assertEquals(1, courseAssignments.Count : Long, 'testUniquenessConstraint)

    passed('testUniquenessConstraint)
  }


  def testIssue68 = {
    //https://github.com/max-l/Squeryl/issues#issue/68
    // Invoking a persisent field during construction causes interference in AST construction
    
    val a = new ASTConstructionInterferenceA
    val bs = a.bs
    val ast = bs.ast.asInstanceOf[QueryExpressionElements]

    val andExp = ast.whereClause.get.asInstanceOf[EqualityExpression]

    assert(andExp.left.isInstanceOf[ConstantExpressionNode[_]], "expected a ConstantExpressionNode[_] in the where clause :\n" + bs.statement)

    bs.deleteAll
    passed('testIssue68)
  }
}

