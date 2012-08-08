package org.squeryl.test.schooldb

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
import java.sql.SQLException
import org.squeryl.annotations.{Column}
import org.squeryl.framework._
import java.util.Date
import java.text.SimpleDateFormat
import org.squeryl.dsl.{GroupWithMeasures}
import org.squeryl.dsl._
import org.squeryl._
import adapters.{MSSQLServer, PostgreSqlAdapter, OracleAdapter, MySQLAdapter, DerbyAdapter}
import internals.{FieldMetaData, FieldReferenceLinker}
import org.scalatest.Suite
import collection.mutable.ArrayBuffer
import org.squeryl.internals.StatementWriter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast.ExpressionNode


object SingleTestRun extends org.scalatest.Tag("SingleTestRun")

class SchoolDbObject {
  var id: Int = 0
}

trait Person

class Student(var name: String, var lastName: String, var age: Option[Int], var gender: Int, var addressId: Option[Int], var isMultilingual: Option[Boolean])
  extends Person {
  
  val id: Int = 0

  override def toString = "Student:" + id + ":" + name
  
  import org.squeryl.PrimitiveTypeMode._
  
  def dummyKey = compositeKey(age, addressId)
}

case class Course2(id: Int, name: String, confirmed: Boolean, occVersionNumber: Int)

case class Course(var name: String, var startDate: Date, var finalExamDate: Option[Date],
  @Column("meaninglessLongZ")
  var meaninglessLong: Long,
  @Column("meaninglessLongOption")
  var meaninglessLongOption: Option[Long], val confirmed: Boolean) {
  
  val id: Int = 0
  
  val occVersionNumber: Int = 0

  def occVersionNumberZ = occVersionNumber

  override def toString = "Course:" + id + ":" + name

  var rawData = {
    val a = new Array[Byte](1)
    a(0) = 5
    a
  }
}

class CourseSubscription(var courseId: Int, var studentId: Int)
  extends SchoolDbObject {

  override def toString = "CourseSubscription:" + id
}

class CourseAssignment(var courseId: Int, var professorId: Long)
  extends SchoolDbObject {

  override def toString = "CourseAssignment:" + id
}

class Address(var streetName: String, var numberz:Int, var numberSuffix:Option[String], var appNumber: Option[Int], var appNumberSuffix: Option[String])
  extends SchoolDbObject {

  override def toString = "rue " + streetName 
}

class Professor(var lastName: String, var yearlySalary: Float, var weight: Option[Float], var yearlySalaryBD: BigDecimal, var weightInBD: Option[BigDecimal]) extends KeyedEntity[Long] with Person {

  var id: Long = 0
  override def toString = "Professor:" + id + ",sal=" + yearlySalary
}

case class PostalCode(code: String) extends KeyedEntity[String] {
  def id = code
}

case class School(val addressId: Int, val name: String, val parentSchoolId: Long, val transientField: String) extends KeyedEntity[Long] {
  var id_field: Long = 0

  def id = id_field
}

object SDB extends SchoolDb

object Tempo extends Enumeration {
  type Tempo = Value
  val Largo = Value(1, "Largo")
  val Allegro = Value(2, "Allegro")
  val Presto = Value(3, "Presto")
}

class StringKeyedEntity(val id: String, val tempo: Tempo.Tempo) extends KeyedEntity[String] {
  def this() = this("", Tempo.Largo)
}

class SchoolDb extends Schema {

  implicit object personKED extends KeyedEntityDef[Student,Int] {
    def getId(a:Student) = a.id
    def isPersisted(a:Student) = a.id > 0
    def idPropertyName = "id"
  }
  
  implicit object schoolDbObjectKED extends KeyedEntityDef[SchoolDbObject,Int] {
    def getId(a:SchoolDbObject) = a.id
    def isPersisted(a:SchoolDbObject) = a.id > 0
    def idPropertyName = "id"
  }
  
  
  implicit object courseKED extends KeyedEntityDef[Course,Int] {
    def getId(a:Course) = a.id
    def isPersisted(a:Course) = a.id > 0
    def idPropertyName = "id"
    override def optimisticCounterPropertyName = Some("occVersionNumber")
  }
  
  implicit object course2KED extends KeyedEntityDef[Course2,Int] {
    def getId(a:Course2) = a.id
    def isPersisted(a:Course2) = a.id > 0
    def idPropertyName = "id"
    override def optimisticCounterPropertyName = Some("occVersionNumber")
  }
  
  val courses2 = table[Course2]

  import org.squeryl.PrimitiveTypeMode._

//  override val name = {
//    if(Session.currentSession.databaseAdapter.isInstanceOf[OracleAdapter])
//      Some("squeryl")
//    else if(Session.currentSession.databaseAdapter.isInstanceOf[PostgreSqlAdapter])
//      Some("public")
//    else
//      None
//  }

  override val name = None

  override def columnNameFromPropertyName(n:String) =
    NamingConventionTransforms.snakify(n)

  /**
   * Let's illustrate the support for crappy table naming convention !
   */
  override def tableNameFromClassName(n:String) =
    "T_" + n

  val stringKeyedEntities =
    table[StringKeyedEntity]

  val professors = table[Professor]
  
  val students = table[Student] //(implicitly[Manifest[Student]],personKEDO)
  
  val addresses = table[Address]("AddressexageratelyLongName")

  val courses = table[Course]

  val courseSubscriptions = table[CourseSubscription]

  val courseAssigments = table[CourseAssignment]

  val schools = table[School]

  val postalCodes = table[PostalCode]
  
// uncomment to test : when http://www.assembla.com/spaces/squeryl/tickets/14-assertion-fails-on-self-referring-onetomanyrelationship
//  an unverted constraint gets created, unless expr. is inverted : child.parentSchoolId === parent.id
//  val schoolHierarchy =
//    oneToManyRelation(schools, schools).via((parent, child) => parent.id === child.parentSchoolId)

  on(schools)(s => declare(
    s.id_field is (primaryKey),
    s.name is(indexed("uniqueIndexName"), unique),
    s.name defaultsTo("no name"),
    columns(s.name, s.addressId) are(indexed),
    s.parentSchoolId is(indexed, unique)
    //_.addressId is(autoIncremented) currently only supported on KeyedEntity.id ... ! :(
  ))

  on(professors)(p => declare(
    p.lastName is(named("theLastName"))
  ))
  
  on(professors)(p => declare(
    p.yearlySalary is(dbType("real"))
  ))

  on(stringKeyedEntities)(e => declare(
    e.tempo.defaultsTo(Tempo.Largo)
  ))

  on(schools)(s => declare(
    s.transientField is transient
  ))
  
  // disable the override, since the above is good for Oracle only, this is not a usage demo, but
  // a necessary hack to test the dbType override mechanism and still allow the test suite can run on all database :
  override def columnTypeFor(fieldMetaData: FieldMetaData, owner: Table[_])  =
    if(fieldMetaData.nameOfProperty == "yearlySalary" && Session.currentSession.databaseAdapter.isInstanceOf[OracleAdapter])
      Some("float")
    else
      None


  override def drop = {
    Session.cleanupResources
    super.drop
  }

  val beforeInsertsOfPerson = new ArrayBuffer[Person]
  val beforeInsertsOfKeyedEntity = new ArrayBuffer[KeyedEntity[_]]
  val beforeInsertsOfProfessor = new ArrayBuffer[Professor]
  val afterInsertsOfProfessor = new ArrayBuffer[Professor]
  val afterInsertsOfSchool = new ArrayBuffer[School]
  val beforeDeleteOfSchool = new ArrayBuffer[School]
  val afterDeleteOfSchool = new ArrayBuffer[School]
  //will contain the identityHashCode :
  val professorsCreatedWithFactory = new ArrayBuffer[Int]

  override def callbacks = Seq(

    beforeInsert[Person]
      map(p => {beforeInsertsOfPerson.append(p); p}),

    beforeInsert[Professor]
      call(beforeInsertsOfProfessor.append(_)),

    beforeInsert[KeyedEntity[_]]
      call(beforeInsertsOfKeyedEntity.append(_)),

    afterInsert[Professor]
      call(afterInsertsOfProfessor.append(_)),

    afterInsert(schools)
      call(afterInsertsOfSchool.append(_)),

    beforeDelete(schools) call(beforeDeleteOfSchool.append(_)),

    afterDelete(schools) call(afterDeleteOfSchool.append(_)),

    factoryFor(professors) is {
      val p = new Professor("Prof From Factory !", 80.0F, Some(70.5F), 80.0F, Some(70.5F))
      professorsCreatedWithFactory.append(System.identityHashCode(p))
      p
    }
  )

}

class TestInstance(schema : SchoolDb){
  import schema._
  val oneHutchissonStreet = addresses.insert(new Address("Hutchisson",1, None,None,None))
  val twoHutchissonStreet = addresses.insert(new Address("Hutchisson",2, None,None,None))
  val oneTwoThreePieIXStreet = addresses.insert(new Address("Pie IX",123, None,Some(4),Some("A")))

  val xiao   = students.insert(new Student("Xiao", "Jimbao Gallois", Some(24), 2, Some(oneHutchissonStreet.id), Some(true)))
  val georgi = students.insert(new Student("Georgi", "Balanchivadze Fourrier", Some(52), 1, Some(oneHutchissonStreet.id), None))
  val pratap = students.insert(new Student("Pratap", "Jamsetji Bach", Some(25), 1, Some(oneTwoThreePieIXStreet.id), None))
  val gontran = students.insert(new Student("Gontran", "Plourde", Some(25), 1, Some(oneHutchissonStreet.id), Some(true)))
  val gaitan = students.insert(new Student("Gaitan", "Plouffe", Some(19), 1, None, Some(true)))

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  val jan2009 = dateFormat.parse("2009-01-01")
  val may2009 = dateFormat.parse("2009-05-01")
  val feb2009 = dateFormat.parse("2009-02-01")
  val feb2010 = dateFormat.parse("2010-02-01")
  val feb2011 = dateFormat.parse("2011-02-01")

  val groupTheory = courses.insert(new Course("Group Theory", jan2009, Some(may2009), 0, None, false))
  val heatTransfer = courses.insert(new Course("Heat Transfer", feb2009, None, 3, Some(1234), false))
  val counterpoint = courses.insert(new Course("Counterpoint", feb2010, None,0, None, true))
  val mandarin = courses.insert(new Course("Mandarin 101", feb2010, None, 0, None, true))

  courseSubscriptions.insert(new CourseSubscription(groupTheory.id, xiao.id))
  courseSubscriptions.insert(new CourseSubscription(heatTransfer.id, gontran.id))
  courseSubscriptions.insert(new CourseSubscription(heatTransfer.id, georgi.id))
  courseSubscriptions.insert(new CourseSubscription(counterpoint.id, pratap.id))
  courseSubscriptions.insert(new CourseSubscription(mandarin.id, gaitan.id))

  val tournesol = professors.insert(new Professor("tournesol", 80.0F, Some(70.5F), 80.0F, Some(70.5F)))
}

abstract class FullOuterJoinTests extends SchoolDbTestBase{

  import org.squeryl.PrimitiveTypeMode._
  import schema._



  test("NewLeftOuterJoin1Reverse")  {
    val testInstance = sharedTestInstance; import testInstance._

    //loggerOn

    val leftOuterJoinStudentAddresses =
      join(addresses.leftOuter, students)((a,s) =>
        select((s,a))
        orderBy(s.id)
        on(s.addressId === a.map(_.id))
      )

    val res =
      (for(t <- leftOuterJoinStudentAddresses)
       yield (t._1.id, t._2.map(a=>a.id))).toList

    val expected = List(
      (xiao.id,Some(oneHutchissonStreet.id)),
      (georgi.id,Some(oneHutchissonStreet.id)),
      (pratap.id,Some(oneTwoThreePieIXStreet.id)),
      (gontran.id,Some(oneHutchissonStreet.id)),
      (gaitan.id,None))

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testNewOuterJoin1 )
  }
}

abstract class SchoolDbTestBase extends SchemaTester with QueryTester with RunTestsInsideTransaction {

  lazy val schema = new SchoolDb

  var sharedTestInstance : TestInstance = null

  override def prePopulate() {
    sharedTestInstance = new TestInstance(schema)
  }

}
abstract class SchoolDbTestRun extends SchoolDbTestBase {
  import org.squeryl.PrimitiveTypeMode._
  import schema._

  
  
  
  test("StringKeyedEntities"){
    val testInstance = sharedTestInstance; import testInstance._
    val se = stringKeyedEntities.insert(new StringKeyedEntity("123", Tempo.Largo))
  }

  test("CountSignatures"){
    val testInstance = sharedTestInstance; import testInstance._
    val q =
      from(courseSubscriptions)(cs =>
        compute(countDistinct(cs.courseId))
      )

    //assertEquals(4L, q: Long, 'testCountSignatures)
    (q:Long) should equal(4L)

    val q2 =
      from(courseSubscriptions)(cs =>
        compute(count(cs.courseId))
      )

    //assertEquals(5L, q2: Long, 'testCountSignatures)
    (q2:Long) should equal(5L)

    val q3 =
      from(courseSubscriptions)(cs =>
        compute(count)
      )

    //assertEquals(5L, q3: Long, 'testCountSignatures)
    (q3:Long) should equal(5L)

    //passed('testCountSignatures)
  }

  def avgStudentAge =
    from(students)(s =>
      compute(avg(s.age))
    )

  def avgStudentAgeFunky =
    from(students)(s =>
      compute(avg(s.age), avg(s.age) + 3, avg(s.age) / count, count + 6)
    )

  def addressesOfStudentsOlderThan24 =
    from(students, addresses)((s,a) =>
      where((24 lt s.age) and (24 lt s.age))
      select(&(a.numberz || " " || a.streetName || " " || a.appNumber))
    )

  test("DeepNest1"){
    val testInstance = sharedTestInstance; import testInstance._

    val q = from(professors)(p0 => select(p0))

    val q1 = from(q)(p => where(p.lastName === tournesol.lastName) select(p))

    val profTournesol = q1.single

    assertEquals(tournesol.id, profTournesol.id, 'testDeepNest)

    passed('testDeepNest)
  }

//  test("alternatePKnameForKeyedEntity-issue55") {
//    schools.update(new School(0,"École Bussonière",12))
//  }

  test("KeyedEntityIdRenaming"){

    postalCodes.insert(PostalCode("J0B-2C0"))

    passed('testKeyedEntityIdRenaming)
  }

  test("DeepNest2"){
    val testInstance = sharedTestInstance; import testInstance._

    val q = from(from(from(professors)(p0 => select(p0)))(p1 => select(p1)))(p2 => select(p2))

    val q1 = from(q)(p => where(p.lastName === tournesol.lastName) select(p))

    val profTournesol = q1.single

    assertEquals(tournesol.id, profTournesol.id, 'testDeepNest)

    passed('testDeepNest)
  }

  test("assertColumnNameChangeWithDeclareSyntax") {
    val st = Session.currentSession.connection.createStatement()
    val r = st.execute("select the_Last_Name from t_professor")                                                        
    // this should not blow up...
  }
  
  test("OptionStringInWhereClause"){
    val testInstance = sharedTestInstance; import testInstance._

    val q =
      from(addresses)(a => where(a.appNumberSuffix === Some("A")) select(a))

    val h = q.head

    assertEquals(oneTwoThreePieIXStreet.id, h.id, 'testOptionStringInWhereClause)

    assertEquals(Some("A"), h.appNumberSuffix, 'testOptionStringInWhereClause)

    passed('testOptionStringInWhereClause)
  }

  test("blobTest"){
    val testInstance = sharedTestInstance; import testInstance._

    var c = courses.where(_.id === counterpoint.id).single

    assertEquals(c.rawData(0), 5, 'blobTest)

    c.rawData(0) = 3

    c.update

    c = courses.where(_.id === counterpoint.id).single

    assertEquals(c.rawData(0), 3, 'blobTest)

    val data = Array.fill(2)(2.toByte)
    courses.update(c => where(c.id === counterpoint.id) set(c.rawData := data))
    c = courses.where(_.id === counterpoint.id).single
    assertEquals(2, c.rawData(0), 'blobTest)
    assertEquals(2, c.rawData(1), 'blobTest)

    passed('blobTest)
  }

  test("InOpWithStringList"){
    val testInstance = sharedTestInstance; import testInstance._
    val r =
      from(students)(s=>
        where(s.name in Seq("Xiao", "Georgi"))
        select(s.id)
      ).toSet

    assertEquals(Set(xiao.id,georgi.id), r, 'testInOpWithStringList)

    passed('testInOpWithStringList)
  }
  
  test("transient annotation", SingleTestRun) {
    

    val s = schools.insert(new School(123,"EB123",0, "transient !"))
    
    val s2 = schools.lookup(s.id).get
    
    assert(s.id == s2.id)
    
    assert(s2.transientField != "transient !")
    
  }
  
  test("lifecycleCallbacks") {


    beforeInsertsOfPerson.clear
    beforeInsertsOfKeyedEntity.clear
    beforeInsertsOfProfessor.clear
    afterInsertsOfProfessor.clear
    beforeDeleteOfSchool.clear
    professorsCreatedWithFactory.clear

    val s1 = students.insert(new Student("z1", "z2", Some(4), 1, Some(4), Some(true)))

    assert(beforeInsertsOfPerson.exists(_ == s1))
    assert(! beforeInsertsOfKeyedEntity.exists(_ == s1))
    assert(!beforeInsertsOfProfessor.exists(_ == s1))
    assert(!afterInsertsOfProfessor.exists(_ == s1))

    val s2 = schools.insert(new School(0,"EB",0, ""))

    assert(!beforeInsertsOfPerson.exists(_ == s2))
    assert(beforeInsertsOfKeyedEntity.exists(_ == s2))
    assert(!beforeInsertsOfProfessor.exists(_ == s2))
    assert(!afterInsertsOfProfessor.exists(_ == s2))
    assert(afterInsertsOfSchool.exists(_ == s2))

    schools.delete(s2.id)
    assert(beforeDeleteOfSchool.exists(_ == s2))
    assert(afterDeleteOfSchool.exists(_ == s2))

    val s3 = professors.insert(new Professor("z",3.0F,Some(2),BigDecimal(3),Some(BigDecimal(3))))

    assert(beforeInsertsOfPerson.exists(_ == s3))
    assert(beforeInsertsOfKeyedEntity.exists(_ == s3))
    assert(beforeInsertsOfProfessor.exists(_ == s3))
    assert(afterInsertsOfProfessor.exists(_ == s3))

    assert(professors.allRows.map(System.identityHashCode(_)).toSet == professorsCreatedWithFactory.toSet)
  }


  test("MetaData"){

    val testInstance = sharedTestInstance; import testInstance._

    professors.posoMetaData.primaryKey.get.left.get

    val tst = new Student("Xiao", "Jimbao Gallois", Some(24), 2,Some(1), None)
    val fmd = addresses.posoMetaData.findFieldMetaDataForProperty("appNumberSuffix")
    assert(fmd.get.fieldType.isAssignableFrom(classOf[String]), "'FieldMetaData " + fmd + " should be of type java.lang.String")

    val pk = addresses.posoMetaData.primaryKey.get.left.get
    assert(pk != None, "MetaData of addresses should have 'id' as PK : \n" + addresses.posoMetaData)

    passed('testMetaData )
  }

  test("OptionAndNonOptionMixInComputeTuple"){
    val t:Product4[Option[Float],Option[Float],Option[Double], Long] = avgStudentAgeFunky
    passed('testOptionAndNonOptionMixInComputeTuple)
  }

  test("testServerSideFunctionCall") {

    val s =
      from(students)(s =>
        where(lower(s.name) === lower("GONtran"))
        select((&(lower(s.name)), &(upper("zaza"))))
      ).single

    assertEquals("gontran", s._1, 'testServerSideFunctionCall)
    assertEquals("ZAZA", s._2, 'testServerSideFunctionCall)

    passed('testServerSideFunctionCall)
  }

  test("ConcatWithOptionalCols"){
    val dbAdapter = Session.currentSession.databaseAdapter
    if(!dbAdapter.isInstanceOf[MSSQLServer] && !dbAdapter.isInstanceOf[DerbyAdapter]) {
      // concat doesn't work in Derby with numeric fields.
      // see: https://issues.apache.org/jira/browse/DERBY-1306

      val res = addressesOfStudentsOlderThan24.toList

      passed('testConcatWithOptionalCols )
    }
  }

  test("ScalarOptionQuery"){
    val avgAge:Option[Float] = avgStudentAge
    //println("avgAge = " + avgAge)
    passed('testScalarOptionQuery )
  }

  test("LikeOperator"){
    val testInstance = sharedTestInstance; import testInstance._
    val q =
      from(students)(s=>
        where(s.name like "G%")
        select(s.id)
        orderBy(s.name)
      )

    validateQuery('testLikeOperator, q, identity[Int], List(gaitan.id,georgi.id,gontran.id))
    
  }

  test("SingleOption"){
    val testInstance = sharedTestInstance; import testInstance._
    val q =
      from(students)(s=>
        where(s.name like "G%")
        select(s.id)
        orderBy(s.name)
      )
      
    val shouldBeRight =
      try {
        Left(q.singleOption)
      }
      catch {
        case e: Exception => Right(e)
      }

    assert(shouldBeRight.isRight, "singleOption did not throw an exception when it should have") 

    val q2 =
      from(students)(s=>
        where(s.name like "Gontran")
        select(s.id)
        orderBy(s.name)
      )
    
    q2.singleOption should equal(Some(gontran.id))
  }  
    
  test("isNull and === None comparison"){  
    val z1 =
      from(students)(s=>
        where({
          //TODO: REFACTOR Z
          s.isMultilingual === (None :Option[Boolean])
        })
        select(s.id)
      )
    
    val z2 =
      from(students)(s=>
        where({
          val a = s.isMultilingual.isNull
          a
          })
        select(s.id)
      )
                
      val r1 = z1.toSet
      val r2 = z2.toSet
      
    assertEquals(r1, r2, "result of isNull and === None differ")      
  }

//  test("NotOperator"){
//    val testInstance = sharedTestInstance; import testInstance._
//    val q =
//      from(students)(s=>
//        where(not(s.name like "G%"))
//        select(s.id)
//        orderBy(s.name desc)
//      )
//
//    validateQuery('testNotOperator, q, identity[Int], List(xiao.id, pratap.id))
//  }

  test("DateTypeMapping"){
    val testInstance = sharedTestInstance; import testInstance._

    val mandarinCourse =
      courses.where(c => c.id === mandarin.id).single

    assert(mandarinCourse.startDate == feb2010,
      'testDateTypeMapping + " failed, expected " + feb2010 + " got " + mandarinCourse.startDate)

    mandarinCourse.startDate = feb2011

    mandarinCourse.update

    val mandarinCourse2011 =
      courses.where(c => c.id === mandarin.id).single

    assert(mandarinCourse.startDate == feb2011,
      'testDateTypeMapping + " failed, expected " + feb2011 + " got " + mandarinCourse.startDate)

    passed('testDateTypeMapping )
  }

  test("DateOptionMapping"){
    val testInstance = sharedTestInstance; import testInstance._

    var groupTh =
      courses.where(c => c.id === groupTheory.id).single

    assert(groupTh.finalExamDate == Some(may2009),
      'testDateOptionMapping + " failed, expected " + Some(may2009) + " got " + groupTh.finalExamDate)


    // test date update :
    groupTh.finalExamDate = Some(feb2011)

    groupTh.update

    groupTh =
      courses.where(c => c.id === groupTheory.id).single

    assert(groupTh.finalExamDate == Some(feb2011),
      'testDateOptionMapping + " failed, expected " + Some(feb2011) + " got " + groupTh.finalExamDate)


    // test date update to null :

    groupTh.finalExamDate = None

    groupTh.update

    groupTh =
      courses.where(c => c.id === groupTheory.id).single

    assert(groupTh.finalExamDate == None,
      'testDateOptionMapping + " failed, expected " + None + " got " + groupTh.finalExamDate)


    // test date update from None to Some :

    groupTh.finalExamDate = Some(may2009)

    groupTh.update

    groupTh =
      courses.where(c => c.id === groupTheory.id).single

    assert(groupTh.finalExamDate == Some(may2009),
      'testDateOptionMapping + " failed, expected " + Some(may2009) + " got " + groupTh.finalExamDate)

    passed('testDateOptionMapping )
  }

  test("DateComparisonInWhereClause"){
    val testInstance = sharedTestInstance; import testInstance._

//    val feb2010 = dateFormat.parse("2010-02-01")
// ...
//    val groupTheory = courses.insert(new Course("Group Theory", jan2009, Some(may2009), 0, None, false))
//    val heatTransfer = courses.insert(new Course("Heat Transfer", feb2009, None, 3, Some(1234), false))
//    val counterpoint = courses.insert(new Course("Counterpoint", feb2010, None,0, None, true))
//    val mandarin = courses.insert(new Course("Mandarin 101", feb2010, None, 0, None, true))

    val jan2010 = dateFormat.parse("2010-01-01")
    val mar2010 = dateFormat.parse("2010-03-01")

    val mandarinAndCounterpointCourses =
      from(courses)(c=>
        where(c.startDate > jan2010 and c.startDate < mar2010)
        select(c)
        orderBy(List[ExpressionNode](c.startDate.asc, c.id.asc))
      ).toList

    val expected = List(counterpoint.id,  mandarin.id)
    val result = mandarinAndCounterpointCourses.map(c=>c.id)

    assert(expected == result,
      'testDateComparisonInWhereClause + " expected " + expected + " got " + result)

    passed('testDateComparisonInWhereClause )
  }

  test("DateOptionComparisonInWhereClause"){
     val testInstance = sharedTestInstance; import testInstance._
//    val jan2009 = dateFormat.parse("2009-01-01")
//...
//    val groupTheory = courses.insert(new Course("Group Theory", jan2009, Some(may2009), 0, None, false))
//    val heatTransfer = courses.insert(new Course("Heat Transfer", feb2009, None, 3, Some(1234), false))
//    val counterpoint = courses.insert(new Course("Counterpoint", feb2010, None,0, None, true))
//    val mandarin = courses.insert(new Course("Mandarin 101", feb2010, None, 0, None, true))

    val jan2008 = dateFormat.parse("2008-01-01")

    //Session.currentSession.setLogger(s => println(s))

    val result1 =
      from(courses)(c=>
        where(c.finalExamDate >= Option(jan2008) and c.finalExamDate.isNotNull)
        select(c)
        orderBy(c.finalExamDate, c.id asc)
      ).toList.map(c=>c.id)

    val result2 =
      from(courses)(c=>
        where(c.finalExamDate <= Some(jan2009))
        select(c)
        orderBy(c.finalExamDate, c.id asc)
      ).toList.map(c=>c.id)

    val result3 =
      from(courses)(c=>
        where(c.finalExamDate >= Some(feb2009))
        select(c)
        orderBy(c.finalExamDate, c.id asc)
      ).toList.map(c=>c.id)

    val expected = List(groupTheory.id)

    assert(expected == result1,
      'testDateOptionComparisonInWhereClause + " expected " + expected + " got " + result1)

    assert(Nil == result2,
      'testDateOptionComparisonInWhereClause + " expected " + expected + " got " + result2)

    assert(expected == result3,
      'testDateOptionComparisonInWhereClause + " expected " + expected + " got " + result3)

    passed('testDateOptionComparisonInWhereClause )
  }

  test("NVLFunction"){
    val testInstance = sharedTestInstance; import testInstance._

//    val groupTheory = courses.insert(new Course("Group Theory", jan2009, Some(may2009), 0, None, false))
//    val heatTransfer = courses.insert(new Course("Heat Transfer", feb2009, None, 3, Some(1234), false))
//    val counterpoint = courses.insert(new Course("Counterpoint", feb2010, None,0, None, true))
//    val mandarin = courses.insert(new Course("Mandarin 101", feb2010, None, 0, None, true))

    //Session.currentSession.setLogger(s => println(s))

    val result =
      from(courses)(c=>
        where(nvl(c.meaninglessLongOption, 3) <> 1234 and nvl(c.meaninglessLongOption, 3) === 3)
        select(&(nvl(c.meaninglessLongOption, 5)))
      ).toList : List[Long]

    val expected = List(5,5,5)

    assert(expected == result,
      'testNVLFunction + " expected " + expected + " got " + result)

    passed('testNVLFunction )
  }

  test("LongTypeMapping"){
    val testInstance = sharedTestInstance; import testInstance._

    var ht = courses.where(c => c.id === heatTransfer.id).single

    assert(ht.meaninglessLong == 3, "expected 3, got " + ht.meaninglessLong)
    assert(ht.meaninglessLongOption == Some(1234), "expected Some(1234), got " + ht.meaninglessLongOption)

    ht.meaninglessLong = -3
    ht.meaninglessLongOption = None

    ht.update

    ht = courses.where(c => c.id === heatTransfer.id).single

    assert(ht.meaninglessLong == -3, "expected -3, got " + ht.meaninglessLong)
    assert(ht.meaninglessLongOption == None, "expected None, got " + ht.meaninglessLongOption)

    ht.meaninglessLongOption = Some(4321)

    ht.update

    ht = courses.where(c => c.id === heatTransfer.id).single

    assert(ht.meaninglessLongOption == Some(4321), "expected Some(4321), got " + ht.meaninglessLongOption)

    ht.meaninglessLongOption = Some(1234)

    ht.update

    assert(ht.meaninglessLongOption == Some(1234), "expected Some(1234), got " + ht.meaninglessLongOption)

    passed('testLongTypeMapping)
  }

  test("BooleanTypeMapping"){
    val testInstance = sharedTestInstance; import testInstance._

    var ht = courses.where(c => c.id === heatTransfer.id).single

    assert(! ht.confirmed, "expected false, got " + ht.confirmed)

//    ht.confirmed = true
//    courses.update(ht)

    update(courses)(c =>
      where(c.id === heatTransfer.id)
      set(c.confirmed := true)
    )

    ht = courses.where(c => c.id === heatTransfer.id).single
    assert(ht.confirmed, "expected true, got " + ht.confirmed)

//    ht.confirmed = false
//    courses.update(ht)

    update(courses)(c =>
      where(c.id === heatTransfer.id)
      set(c.confirmed := false)
    )

    ht = courses.where(c => c.id === heatTransfer.id).single

    assert(! ht.confirmed, "expected false, got " + ht.confirmed)

    passed('testBooleanTypeMapping)
  }

  test("BooleanOptionMapping"){
    val testInstance = sharedTestInstance; import testInstance._

    //println(students.where(s => s.id === gontran.id).dumpAst)

    var g = students.where(s => s.id === gontran.id).single

    assert(g.isMultilingual.get, "expected Some(true), got " + g.isMultilingual)

    g.isMultilingual = None
    g.update
    g = students.where(s => s.id === gontran.id).single
    assert(g.isMultilingual == None, "expected None, got " + g.isMultilingual)

    g.isMultilingual = Some(false)
    g.update
    g = students.where(s => s.id === gontran.id).single
    assert(! g.isMultilingual.get, "expected Some(false), got " + g.isMultilingual)

    g.isMultilingual = Some(true)
    g.update
    g = students.where(s => s.id === gontran.id).single
    assert(g.isMultilingual.get, "expected Some(true), got " + g.isMultilingual)

    passed('testBooleanOptionMapping)
  }

  test("FloatType"){
    val testInstance = sharedTestInstance; import testInstance._

    var t = professors.where(p => p.id === tournesol.id).single

    assert(t.yearlySalary == 80.0, "expected 80.0, got " + t.yearlySalary)
    assert(t.weight == Some(70.5), "expected Some(70.5), got " + t.weight)

    t.yearlySalary = 90.5F
    t.weight = Some(75.7F)
    t.update
    t = professors.where(p => p.id === tournesol.id).single
    assert(t.yearlySalary == 90.5, "expected 90.5, got " + t.yearlySalary)
    assert(t.weight == Some(75.7F), "expected Some(75.7), got " + t.weight)

    t.weight = None
    t.update
    t = professors.where(p => p.id === tournesol.id).single
    assert(t.weight == None, "expected None, got " + t.weight)

    t.yearlySalary = 80.0F
    t.weight = Some(70.5F)
    professors.update(t)
    t = professors.where(p => p.id === tournesol.id).single
    assert(t.yearlySalary == 80.0, "expected 80.0, got " + t.yearlySalary)
    assert(t.weight == Some(70.5), "expected Some(70.5), got " + t.weight)

    passed('testFloatType)
  }

  test("ForUpdate") {
    val testInstance = sharedTestInstance; import testInstance._
    var t = professors.where(p => p.id === tournesol.id).forUpdate.single

    assert(t.yearlySalary == 80.0, "expected 80.0, got " + t.yearlySalary)
    assert(t.weight == Some(70.5), "expected Some(70.5), got " + t.weight)

    passed('testForUpdate)
  }



  test("PartialUpdate1") {
    val testInstance = sharedTestInstance; import testInstance._

    val initialHT = courses.where(c => c.id === heatTransfer.id).single

    val q =
      from(courses)(c =>
        select((c.id, c.meaninglessLong, c.meaninglessLongOption))
        orderBy(c.id)
      )

    val b4 = q.toList

    var nRows = courses.update(c =>
       where(c.id gt -1)
       set(c.meaninglessLong := 123L,
           c.meaninglessLongOption :=  c.meaninglessLongOption + 456L)
              // when meaninglessLongOption is null,the SQL addition will have a null result
    )

    val expectedAfter = List((1,123,None), (2,123,Some(1690)), (3,123,None), (4,123,None))
    val after = q.toList

    assert(nRows == 4)
    assert(expectedAfter == after, "expected " + expectedAfter + " got " + after)

    // alternative syntax :
    nRows =
      update(courses)(c =>
        where(c.id gt -1)
        set(c.meaninglessLong := 0L,
            c.meaninglessLongOption :=  c.meaninglessLongOption - 456L)
      )

    assert(nRows == 4)

    courses.forceUpdate(initialHT)

    val afterReset = q.toList

    assert(b4 == afterReset, "expected " + afterReset + " got " + b4)

    passed('testPartialUpdate1)
  }

  test("PartialUpdateWithInclusionOperator ") {

    update(courses)(c =>
      where(c.id in from(courses)(c0=> where(c0.id lt -1) select(c0.id)))
      set(c.meaninglessLong := 0L,
          c.meaninglessLongOption :=  c.meaninglessLongOption - 456L)
    )

    passed('testPartialUpdateWithInclusionOperator)
  }

  test("HavingClause") {
    //The query here doesn't make much sense, we just test that valid SQL gets generated :
    val q =
      from(professors)(p=>
        groupBy(p.id, p.yearlySalary)
        having(p.yearlySalary gt 75.0F)
      )

    assert(q.statement.indexOf("Having") != -1)
    q.toList

    passed('testHavingClause)
  }

  test("HavingClause2") {
    //The query here doesn't make much sense, we just test that valid SQL gets generated :
    val q =
      from(professors)(p=> {
        val v1 = groupBy(p.id, p.yearlySalary)

        val v2 = v1.having(p.yearlySalary gt 75.0F)


        val v3 = v2.compute(avg(p.yearlySalary))

        v3
      }
      )
    q.toList

    assert(q.statement.indexOf("Having") != -1)
  }

  test("PartialUpdateWithSubQueryInSetClause") {
    val testInstance = sharedTestInstance; import testInstance._

    val zarnitsyn = professors.insert(new Professor("zarnitsyn", 60.0F, Some(70.5F), 60.0F, Some(70.5F)))

    val before = professors.where(p => p.id === tournesol.id).single.yearlySalary

    val expected:Float = from(professors)(p0=> where(tournesol.id === p0.id or p0.id === zarnitsyn.id) compute(nvl(avg(p0.yearlySalary), 123)))

    val c = update(professors)(p =>
      where(p.id === tournesol.id)
      set(p.yearlySalary := from(professors)(p0=> where(p.id === p0.id or p0.id === zarnitsyn.id) compute(nvl(avg(p0.yearlySalary), 123))))
    )

    val after = professors.where(p => p.id === tournesol.id).single.yearlySalary

    assertEquals(expected, after, 'testPartialUpdateWithSubQueryInSetClause)

    update(professors)(p =>
      where(p.id === tournesol.id)
      set(p.yearlySalary := 80.0F)
    )

    professors.delete(zarnitsyn.id)

    passed('testPartialUpdateWithSubQueryInSetClause)
  }

  test("OptimisticCC1") {
    val testInstance = sharedTestInstance; import testInstance._

    Session.currentSession.connection.commit // we commit to release all locks

    var ht = courses.where(c => c.id === heatTransfer.id).single

    transaction {
      var ht2 = courses.where(c => c.id === heatTransfer.id).single
      ht2.update
    }

    var ex: Option[StaleUpdateException] = None
    try {
      ht.update
    }
    catch {
      case e:StaleUpdateException => ex = Some(e)
    }

    ex.getOrElse(org.squeryl.internals.Utils.throwError("StaleUpdateException should have get thrown on concurrent update test."))

    val expectedVersionNumber = ht.occVersionNumberZ + 1

    val actualVersionNumber =
      from(courses)(c => where(c.id === heatTransfer.id) select(c)).single.occVersionNumberZ

    assertEquals(expectedVersionNumber, actualVersionNumber, "optimistic CC failed")

    passed('testOptimisticCC1)
  }

  test("BatchInserts1") {
    val testInstance = sharedTestInstance; import testInstance._
    addresses.insert(List(
      new Address("St-Dominique",14, None,None,None),
      new Address("St-Urbain",23, None,None,None),
      new Address("Sherbrooke",1123, None,Some(454),Some("B"))
    ))

    addresses.insert(List(
      new Address("Van Horne",14, None,None,None)
    ))

    val streetNames = List("Van Horne", "Sherbrooke", "St-Urbain", "St-Dominique")

    val q = addresses.where(a => a.streetName in streetNames)

    assertEquals(4, q.Count : Long, "batched update test failed")

    addresses.delete(q)

    assertEquals(0, q.Count : Long, "batched update test failed")

    passed('testBatchInserts1)
  }

  test("BatchUpdate1") {
    
    val testInstance = sharedTestInstance; import testInstance._
    import schema._
        
    addresses.insert(List(
      new Address("St-Dominique",14, None,None,None),
      new Address("St-Urbain",23, None,None,None),
      new Address("Sherbrooke",1123, None,Some(454),Some("B"))
    ))

    addresses.insert(List(
      new Address("Van Horne",14, None,None,None)
    ))

    val streetNames = List("Van Horne", "Sherbrooke", "St-Urbain", "St-Dominique")

    val q = addresses.where(a => a.streetName in streetNames)

    addresses.update(q.map(a =>{a.streetName += "Z"; a}))

    val updatedStreetNames = List("Van HorneZ", "SherbrookeZ", "St-UrbainZ", "St-DominiqueZ")

    val updatedQ = addresses.where(a => a.streetName in updatedStreetNames)

    assertEquals(4, updatedQ.Count : Long, "batched update test failed")

    addresses.delete(updatedQ)

    assertEquals(0, updatedQ.Count : Long, "batched update test failed")

    passed('testBatchUpdate1)
  }
  
  test("BatchUpdateAndInsert2") {
    
    val testInstance = sharedTestInstance; import testInstance._
    import schema._
    
    
    courses2.insert(
        Seq(Course2(0, "Programming 101", false, 0),
            Course2(0, "Programming 102", false, 0)))
    
    val c = courses2.where(_.name like "Programming %")
    val c0 = c.toList
    
    assert(c0.size == 2)
    assert(c0.filter(_.confirmed).size == 0)

    courses2.update(c0.map(_.copy(confirmed = true)))
    
    assert(c.filter(_.confirmed).size == 2)
    
    passed('BatchUpdateAndInsert2)
  }

  test("BigDecimal") {
    val testInstance = sharedTestInstance; import testInstance._

    val pt = professors.where(_.yearlySalaryBD.between(75, 80))

    assertEquals(1, pt.Count : Long, 'testBigDecimal)

    assertEquals(tournesol.id, pt.single.id, 'testBigDecimal)


    val babaZula = professors.insert(new Professor("Baba Zula", 80.0F, Some(70.5F), 80.0F, Some(260.1234567F : BigDecimal)))

    update(professors)(p=>
      where(p.id === babaZula.id)
      set(p.weightInBD := Some(261.123456111 : BigDecimal))
    )

    val babaZula2 = professors.where(_.weightInBD === Some(261.123456111: BigDecimal))

    assertEquals(261.123456111, babaZula2.single.weightInBD.get, 'testBigDecimal)

    update(professors)(p=>
      where(p.id === babaZula.id)
      set(p.weightInBD := Some(261.1234561112 : BigDecimal))
    )

    val babaZula3 = professors.where(_.weightInBD === Some(261.1234561112: BigDecimal))

    assertEquals(1, babaZula3.Count : Long, 'testBigDecimal)

    update(professors)(p=>
      where(p.id === babaZula.id)
      set(p.weightInBD := p.weightInBD plus 10 minus 5 times 4 div 2) // FIXME: mulitiplications aren't done first
    )

    val babaZula4 = professors.where(_.weightInBD === Some(532.2469122224: BigDecimal))

    assertEquals(532.2469122224, babaZula4.single.weightInBD.get, 'testBigDecimal)
    assertEquals(1, babaZula4.Count : Long, 'testBigDecimal)

    update(professors)(p=>
      where(p.id === babaZula.id)
      set(p.yearlySalaryBD := p.yearlySalaryBD plus 10 minus 5 times 4 div 2) // FIXME: multiplications aren't done first
    )

    val babaZula5 = professors.where(_.yearlySalaryBD === 170)

    assertEquals(170, babaZula5.single.yearlySalaryBD, 'testBigDecimal)
    assertEquals(1, babaZula5.Count : Long, 'testBigDecimal)
  }

  test("YieldInspectionResidue") {
    val testInstance = sharedTestInstance; import testInstance._

    val z = from(students)(s => where(s.lastName === "Jimbao Gallois") select(s.name)).single

    val r = FieldReferenceLinker.takeLastAccessedFieldReference

    assert(r == None, "!!!!!!!!!!!!")

    passed('testYieldInspectionResidue)
  }

  test("InWithCompute") {
    val testInstance = sharedTestInstance; import testInstance._
    val z0 =
      from(students)(s2 =>
        where(s2.age gt 0)
        compute(min(s2.age))
      )

    val q2 = (z0 : Query[Measures[Option[Int]]] ):  Query[Option[Int]]

    val q3 =
      from(students)(s =>
        where(s.age.isNotNull and s.age.in(q2))
        select(s)
      )

    val res = q3.single

    assertEquals(5, res.id,'testInWithCompute)
    //println("------------->" + res.id)
    passed('testInWithCompute)
  }

  test("IsNotNullWithInhibition") {
    val testInstance = sharedTestInstance; import testInstance._
    val q =
      from(students)(s =>
        where(s.id.isNull.inhibitWhen(true)) // should return all students
        select(s)
      )

    val allStuents = students.allRows.map(_.id).toSet
    val allStudentsQ = q.map(_.id).toSet

    assertEquals(allStuents, allStudentsQ, 'testIsNotNullWithInhibition)


    val q2 =
      from(students)(s =>
        where(s.id.isNull.inhibitWhen(false)) // should return all students
        select(s)
      )

    assertEquals(0, q2.size, 'testIsNotNullWithInhibition)

    passed('testInWithCompute)
  }
  
  test("NewJoin1") {
    val testInstance = sharedTestInstance; import testInstance._
      val q =
       join(students, addresses.leftOuter, addresses)((s,a1,a2) => {

         val s0: Student = s
         val  z0: Option[Address] = a1
         //val  z1: Address = a1
         val  z2: Address = a2
         select(s,a1,a2).
         on(s.addressId === a1.map(_.id), s.addressId === a2.id)
       })

    passed('testNewJoin1)
  }

  test("NewLeftOuterJoin1")  {
    val testInstance = sharedTestInstance; import testInstance._

    //loggerOn

    val leftOuterJoinStudentAddresses =
      join(students, addresses.leftOuter)((s,a) =>
        select((s,a))
        orderBy(s.id)
        on(s.addressId === a.map(_.id))
      )

    val res =
      (for(t <- leftOuterJoinStudentAddresses)
       yield (t._1.id, t._2.map(a=>a.id))).toList

    val expected = List(
      (xiao.id,Some(oneHutchissonStreet.id)),
      (georgi.id,Some(oneHutchissonStreet.id)),
      (pratap.id,Some(oneTwoThreePieIXStreet.id)),
      (gontran.id,Some(oneHutchissonStreet.id)),
      (gaitan.id,None))

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testNewOuterJoin1 )
  }


  test("#62 CompositeKey with Option members generate sql with = null instead of is null")  {
    
    val testInstance = sharedTestInstance; import testInstance._
    // this should not blow up :
    val q = students.where(_.dummyKey === (None: Option[Int], None: Option[Int]))

    q.toList
  }

  test("NewLeftOuterJoin2")  {
    val testInstance = sharedTestInstance; import testInstance._

    //loggerOn

    val leftOuterJoinStudentAddresses =
      join(students, addresses.leftOuter,addresses.leftOuter)((s,a,a2) =>
        select((s,a,a2))
        orderBy(s.id)
        on(s.addressId === a.map(_.id), s.addressId === a2.map(_.id))
      )

    val res =
      (for(t <- leftOuterJoinStudentAddresses)
       yield (t._1.id, t._2.map(a=>a.id), t._3.map(a=>a.id))).toList

    val expected = List(
      (xiao.id,Some(oneHutchissonStreet.id),Some(oneHutchissonStreet.id)),
      (georgi.id,Some(oneHutchissonStreet.id),Some(oneHutchissonStreet.id)),
      (pratap.id,Some(oneTwoThreePieIXStreet.id),Some(oneTwoThreePieIXStreet.id)),
      (gontran.id,Some(oneHutchissonStreet.id),Some(oneHutchissonStreet.id)),
      (gaitan.id,None,None))

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testNewOuterJoin2 )
  }

  test("Boolean2LogicalBooleanConversion") {
    val testInstance = sharedTestInstance; import testInstance._

    val multilingualStudents = students.where(_.isMultilingual === Option(true)).map(_.id).toSet

    //println(multilingualStudents)
    //List(Student:1:Xiao, Student:4:Gontran, Student:5:Gaitan)

    assert(multilingualStudents == Set(xiao.id,gontran.id,gaitan.id))

    passed('testBoolean2LogicalBooleanConversion)
  }

  test("AvgBigDecimal") {
    val testInstance = sharedTestInstance; import testInstance._

    val avgSalary: Option[BigDecimal] =
      from(professors)(p=>
        compute(avg(p.yearlySalaryBD))
      )

    val avgWeight: Option[BigDecimal] =
      from(professors)(p=>
        compute(avg(p.weightInBD))
      )


    val expectedAvgSal_ = professors.allRows.map(_.yearlySalaryBD.doubleValue)

    val expectedAvgSal = expectedAvgSal_.sum / expectedAvgSal_.size

    val expectedAvgWeight_ = professors.allRows.map(_.weightInBD).filter(_ != None).map(_.get)

    val expectedAvgWeight = expectedAvgWeight_.sum / expectedAvgWeight_.size


    assert((expectedAvgSal - avgSalary.get.doubleValue) < 0.01, 'testAvgBigDecimal)
    assert((expectedAvgWeight - avgWeight.get.doubleValue) < 0.01, 'testAvgBigDecimal)


    passed('testAvgBigDecimal)
  }

  test("NewLeftOuterJoin3")  {
    val testInstance = sharedTestInstance; import testInstance._

    //loggerOn

    val leftOuterJoinStudentAddressesAndCourseSubs =
      join(students, addresses.leftOuter,courseSubscriptions)((s,a,cs) =>
        select((s,a,cs))
        orderBy(s.id, cs.courseId)
        on(s.addressId === a.map(_.id), s.id === cs.studentId)
      )

    //println(leftOuterJoinStudentAddressesAndCourseSubs.statement)

    val res =
      (for(t <- leftOuterJoinStudentAddressesAndCourseSubs)
       yield (t._1.id, t._2.map(a=>a.id), t._3.courseId)).toList


    val expected = List(
      (xiao.id,Some(oneHutchissonStreet.id),1),
      (georgi.id,Some(oneHutchissonStreet.id),2),
      (pratap.id,Some(oneTwoThreePieIXStreet.id),3),
      (gontran.id,Some(oneHutchissonStreet.id),2),
      (gaitan.id,None,4))

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testNewOuterJoin3 )
  }

  test("Exists")  {
    val testInstance = sharedTestInstance; import testInstance._
    val studentsWithAnAddress =
      from(students)(s =>
        where(exists(from(addresses)((a) => where(s.addressId === a.id) select(a.id))))
          select(s)
      )

    val res = for (s <- studentsWithAnAddress) yield s.name
    val expected = List("Xiao", "Georgi", "Pratap", "Gontran")

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testExists)
  }

  test("NotExists")  {
    val testInstance = sharedTestInstance; import testInstance._
    val studentsWithNoAddress =
      from(students)(s =>
        where(notExists(from(addresses)((a) => where(s.addressId === a.id) select(a.id))))
        select(s)
      )
    val res = for (s <- studentsWithNoAddress) yield s.name
    val expected = List("Gaitan")

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testNotExists)
  }

  test("VeryNestedExists")  {
    val testInstance = sharedTestInstance; import testInstance._
    val qStudents = from(students) ((s) => select(s))
    val qStudentsFromStudents = from(qStudents) ((s) => select(s))
    val studentsWithAnAddress =
      from(qStudentsFromStudents)(s =>
        where(exists(from(addresses)((a) =>
          where(s.addressId === a.id)
          select(a.id))))
        select(s))

    val res = for (s <- studentsWithAnAddress) yield s.name
    val expected = List("Xiao", "Georgi", "Pratap", "Gontran")

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testVeryNestedExists)

  }

  test("VeryVeryNestedExists"){
    val testInstance = sharedTestInstance; import testInstance._
    val qStudents = from(students) ((s) => select(s))
    val qStudentsFromStudents = from(qStudents) ((s) => select(s))
    val studentsWithAnAddress =
      from(qStudentsFromStudents)(s =>
        where(exists(from(addresses)((a) =>
            where(s.addressId in
              (from(addresses) ( (a2) =>
                where(a2.id === a.id and s.addressId === a2.id)
                select(a2.id))))
            select(a.id))))
          select(s)
      )

    val res = for (s <- studentsWithAnAddress) yield s.name
    val expected = List("Xiao", "Georgi", "Pratap", "Gontran")

    assert(expected == res, "expected :\n " + expected + "\ngot : \n " + res)

    passed('testVeryVeryNestedExists)

  }

  test("UpdateSetAll") {
    val testInstance = sharedTestInstance; import testInstance._
    update(students)(s => setAll(s.age := Some(30)))

    val expected:Long = from(students)(s => compute(count))
    val is:Long = from(students)(s => where(s.age === 30)compute(count))

    assert(expected == is, "expected :\n " + expected + "\ngot : \n " + is)
  }
}

object Issue14Schema extends Schema{
  override def columnNameFromPropertyName(n:String) =
    NamingConventionTransforms.snakify(n)


  val professors = table[Professor]("issue14")
}

abstract class Issue14 extends DbTestBase with QueryTester {

  import org.squeryl.PrimitiveTypeMode._



  test("Issue14"){
    try {
      transaction {
        Session.currentSession.setLogger(println(_))
        val stmt = Session.currentSession.connection.createStatement
        stmt.execute("""create table issue14 (
    yearly_Salary real not null,
    weight_In_B_D decimal(20,16),
    id number primary key not null,
    last_Name varchar2(123) not null,
    yearly_Salary_B_D decimal(20,16) not null,
    weight real
  )
""")

        //stmt.execute("create sequence s_id_issue14")
        val seqName = (new OracleAdapter).createSequenceName(Issue14Schema.professors.posoMetaData.findFieldMetaDataForProperty("id").get)
        try {stmt.execute("create sequence " + seqName)}
        catch {
          case e:SQLException => {} 
        }
      }
      transaction {
        // The problem is that because schema.create wasn't called in this JVM instance, the schema doesn't know
        // that the id should be auto-increment until too late, so id=1 gets inserted.  Then the
        // next one knows about the sequence, so it gets nextval, which is 1, resulting in a uniqueness violation.
        val moriarty = new Professor("Moriarty", 10000000.001f, None, 100, None)
        moriarty.id = 1;
        Issue14Schema.professors.insert(moriarty)
        val xavier = new Professor("Xavier", 10000000.001f, None, 100, None)
        xavier.id = 1;
        Issue14Schema.professors.insert(xavier)
        for (prof <- from(Issue14Schema.professors)(p=>select(p))) {
          println(prof.lastName + " : " + prof.id)
        }
      }
    }
    finally {
      transaction {Issue14Schema.drop}
    }
  }
}
