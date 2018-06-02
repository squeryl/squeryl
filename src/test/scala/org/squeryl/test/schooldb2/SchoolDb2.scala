package org.squeryl.test.schooldb2

import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl._
import dsl.{OneToMany, CompositeKey2}
import java.sql.{Savepoint}

import org.squeryl.framework._

trait SchoolDb2Object {
  val id: Long = 0
}

object SchoolDb2 extends SchoolDb2

class Professor(val lastName: String, var bossId: Option[Long]=None) extends SchoolDb2Object {

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

class Subject(val name: String, val parentSubjectId: Option[Long]) extends SchoolDb2Object {
  
  lazy val courses = SchoolDb2.subjectToCourses.left(this)
  
  lazy val childSubjects = SchoolDb2.subjectToParentSubject.left(this)
  
  lazy val parentSubject = SchoolDb2.subjectToParentSubject.right(this)
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

  implicit object schoolDbObjectKED extends KeyedEntityDef[SchoolDb2Object,Long] {
    def getId(a:SchoolDb2Object) = a.id
    def isPersisted(a:SchoolDb2Object) = a.id > 0
    def idPropertyName = "id"
  }
  
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
    manyToManyRelation(professors, courses, "CourseAssignmentZ").
    via[CourseAssignment]((p,c,a) => (p.id === a.professorId, a.courseId === c.id))

  val courseSubscriptions =
    manyToManyRelation(courses, students).
    via[CourseSubscription]((c,s,cs) => (cs.studentId === s.id, c.id === cs.courseId))

  val subjectToCourses =
    oneToManyRelation(subjects, courses).
    via((s,c) => s.id === c.subjectId)
    
  val bossToProfessors =
    oneToManyRelation(professors, professors).
    via((boss,p) => boss.id === p.bossId)

    
  val subjectToParentSubject =
    oneToManyRelation(subjects, subjects).
    via((subject,childSubject) => Option(subject.id) === childSubject.parentSubjectId)
 
    
  // the default constraint for all foreign keys in this schema :
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) =
    foreignKeyDeclaration.constrainReference

  //now we will redefine some of the foreign key constraints :
  //if we delete a subject, we want all courses to be deleted
  subjectToCourses.foreignKeyDeclaration.constrainReference(onDelete cascade)

  //when a course is deleted, all of the subscriptions will get deleted :
  courseSubscriptions.leftForeignKeyDeclaration.constrainReference(onDelete cascade)

  override def drop = {
    Session.cleanupResources
    super.drop
  }

  val as = table[ASTConstructionInterferenceA]
  val bs = table[ASTConstructionInterferenceB]

  val aToB =
    oneToManyRelation(as, bs).
    via((a, b) => a.id === b.aId)

  aToB.foreignKeyDeclaration.unConstrainReference
}

abstract class SchoolDb2Tests extends SchemaTester with RunTestsInsideTransaction with QueryTester {
  self: DBConnector =>

  val schema = new SchoolDb2
  
  import schema._

  def seedDataDef() = new {
    
    val professeurTournesol = professors.insert(new Professor("Tournesol"))
    val madProfessor = professors.insert(new Professor("Mad Professor"))

    val philosophy = subjects.insert(new Subject("Philosophy", None))
    val chemistry = subjects.insert(new Subject("Chemistry", None))
    val physics = subjects.insert(new Subject("Physic", None))
    val computerScience = subjects.insert(new Subject("Computer Science", None))
    val computationTheory = subjects.insert(new Subject("Computation Theory", Some(computerScience.id)))


    val chemistryCourse = courses.insert(new Course(chemistry.id))
    val physicsCourse = courses.insert(new Course(physics.id))

    val xiaoJimbao = students.insert(new Student("Xiao", "Jimbao"))
  }


//  def testAll = {
//
//    testInFromSeq
//    testInFromSet
//
//    testIssue68
//
//    val entry = entries.insert(Entry("An entry"))
//    val comment = Comment("A single comment")
//    entry.comments.associate(comment)
//
//    from(entry.comments)(c => where(c.id === comment.id) select(c))
//
//    seedData
//
//    testUpdateWithCompositePK
//
//    testCompositeEquality
//
//    testMany2ManyAssociationFromLeftSide
//    testMany2ManyAssociationsFromRightSide
//
//    testOneToMany
//
//    testUniquenessConstraint
//
//    SchoolDb2.drop
//  }

  test("select using query value") {
    seedDataDef()

    val q: Query[String] =
      from(subjects)(s =>
        where(s.name === "Philosophy")
          select(&(from(subjects)(s2 => where(s2.name === s.name) select(s2.name))))
      )

    1 shouldBe q.toList.length
  }

  test("equality using query value") {
    seedDataDef()

    val q: Query[String] =
      from(subjects)(s =>
        where(
          s.name === from(subjects)(s2 => where(s2.name === "Philosophy") select(s2.name))
        )
        select(s.name)
      )

    1 shouldBe q.toList.length
  }

  test("associate comment"){
    val entry = entries.insert(Entry("An entry"))
    val comment = Comment("A single comment")
    entry.comments.associate(comment)

    from(entry.comments)(c => where(c.id === comment.id) select(c))
  }

  test("UpdateWithCompositePK"){
    val seedData = seedDataDef
    import seedData._

    val xiao = {students.lookup(xiaoJimbao.id)
      
    }.get

    val courseSubscription = xiao.courses.assign(chemistryCourse)

    courseSubscriptions.insert(courseSubscription)
    courseSubscription.grade = 95.0F
    courseSubscriptions.update(courseSubscription)

    val cs2 = courseSubscriptions.lookup(courseSubscription.id).get
    
    95.0F shouldBe cs2.grade
  }

  test("Many2ManyAssociationFromLeftSide"){
    val seedData = seedDataDef
    import seedData._

    courseAssignments.Count.toLong shouldBe 0

    professeurTournesol.courses.associate(physicsCourse)

    val c1 = professeurTournesol.courses.single : Course

    c1.id shouldBe  physicsCourse.id

    val ca = professeurTournesol.courses.associations.single : CourseAssignment

    ca.courseId shouldBe  physicsCourse.id

    professeurTournesol.courses.dissociateAll shouldBe 1

    professeurTournesol.courses.dissociateAll shouldBe 0

    courseAssignments.Count.toLong shouldBe 0L
  }

  test("Many2ManyAssociationsFromRightSide"){
    val seedData = seedDataDef
    import seedData._

    courseAssignments.Count.toLong shouldBe 0L

    physicsCourse.professors.associate(professeurTournesol)

    val profT = physicsCourse.professors.single : Professor

    professeurTournesol.lastName shouldBe profT.lastName

    val ca = professeurTournesol.courses.associations.single : CourseAssignment

    ca.courseId shouldBe  physicsCourse.id

    physicsCourse.professors.dissociateAll shouldBe 1

    physicsCourse.professors.dissociateAll shouldBe 0

    courseAssignments.Count.toLong shouldBe 0
  }

  test("OneToMany"){
    val seedData = seedDataDef
    import seedData._

    val philosophyCourse10AMWednesday = new Course
    val philosophyCourse2PMWednesday = new Course
    val philosophyCourse3PMFriday = new Course

    philosophy.courses.associate(philosophyCourse10AMWednesday)
    philosophy.courses.associate(philosophyCourse2PMWednesday)
    philosophy.courses.associate(philosophyCourse3PMFriday)

    philosophy.courses.map(_.id).toSet shouldBe Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id, philosophyCourse3PMFriday.id)
    philosophy.name shouldBe philosophyCourse2PMWednesday.subject.single.name

    // verify that a reassociation does an update and not an insert :
    val pk1 = philosophyCourse3PMFriday.id

    computationTheory.courses.associate(philosophyCourse3PMFriday)

    pk1 shouldBe philosophyCourse3PMFriday.id

    // verify that the reassociation worked, which means that
    // 1) : the set of philosophy.courses was reduced properly
    philosophy.courses.map(_.id).toSet shouldBe Set(philosophyCourse10AMWednesday.id, philosophyCourse2PMWednesday.id)

    // 2) philosophyCourse3PMFriday.subject points to the proper subject
    computationTheory.name shouldBe philosophyCourse3PMFriday.subject.single.name
  }

  test("CompositeEquality"){
    val seedData = seedDataDef
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

    val qA3 =
      courseAssignments.where(_.id === a.id)

    _existsAndEquals(qA3.headOption, a)

    courseAssignments.delete(compositeKey(a.courseId, a.professorId))
    qA3.Count.toLong shouldBe 0L
  }

  private def _existsAndEquals(oca: Option[CourseAssignment], ca: CourseAssignment) = {

    if(oca == None)
      org.squeryl.internals.Utils.throwError("query returned no rows")

    ca.id shouldBe oca.get.id
  }

  test("UniquenessConstraint"){
    val seedData = seedDataDef
    import seedData._

    courseAssignments.Count.toLong shouldBe 0

    physicsCourse.professors.associate(professeurTournesol)
    courseAssignments.Count.toLong shouldBe 1

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
      org.squeryl.internals.Utils.throwError('testUniquenessConstraint + " failed, unique constraint violation occured")

    courseAssignments.Count.toLong shouldBe 1
  }


//  test("Issue68"){
//    //https://github.com/squeryl/squeryl/issues#issue/68
//    // Invoking a persisent field during construction causes interference in AST construction
//
//    val a = new ASTConstructionInterferenceA
//    val bs = a.bs
//    val ast = bs.ast.asInstanceOf[QueryExpressionElements]
//
//    val andExp = ast.whereClause.get.asInstanceOf[EqualityExpression]
//
//    assert(andExp.left.isInstanceOf[ConstantExpressionNode[_]], "expected a ConstantExpressionNode[_] in the where clause :\n" + bs.statement)
//
//    bs.deleteAll
//    passed('testIssue68)
//  }

  test("InFromSet"){
    val set = Set("foo", "bar", "baz")
    from(entries)(e => where(e.text.in(set))select(e)).toList
  }

  test("InFromSeq"){
    val set = Set("foo", "bar", "baz").toSeq
    from(entries)(e => where(e.text.in(set))select(e)).toList
  }
  
  test("Inequality with query on right hand side", SingleTestRun) {
    val seedData = seedDataDef
    import seedData._
   
    val xiao = students.lookup(xiaoJimbao.id).get

    val courseSubscription = xiao.courses.assign(chemistryCourse)

    courseSubscriptions.insert(courseSubscription)
    courseSubscription.grade = 95.0F
    courseSubscriptions.update(courseSubscription)

    val cs2 = courseSubscriptions.lookup(courseSubscription.id).get
    
    95.0F shouldBe cs2.grade
    
    from(courseSubscriptions)(p => compute(avg(p.grade)))
   
    val belowOrEqualToAvg = 
      from(courseSubscriptions)(p =>
        where(p.grade lte from(courseSubscriptions)(p => compute(avg(p.grade))))
        select(p)
      ).toList
    belowOrEqualToAvg should have size 1
    
    val belowAvg = 
      from(courseSubscriptions)(p =>
        where(p.grade lt from(courseSubscriptions)(p => compute(avg(p.grade))))
        select(p)
      ).toList
    belowAvg shouldBe empty
  }
  
  test ("#73 relations with Option[] on one side of the equality expression blow up") {
    seedDataDef()
        
    val cs = subjects.where(_.name === "Computer Science").single
    cs.childSubjects.single.name shouldBe "Computation Theory"
  }
}

