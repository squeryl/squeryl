package org.squeryl.test

import org.squeryl._
import org.squeryl.framework.{SchemaTester, RunTestsInsideTransaction}
import org.squeryl.dsl.ast._
import org.squeryl.PrimitiveTypeMode._

object LogicalBooleanObjTests {

  class Dummy(val id:Int, val p1:Int, val p2:Int) extends KeyedEntity[Int]

  object TestSchema extends Schema {
    val dummy = table[Dummy]
  }

}


abstract class LogicalBooleanObjTests extends SchemaTester with RunTestsInsideTransaction{
  import org.squeryl.test.LogicalBooleanObjTests._

  final def schema = TestSchema

  test("and operation") {
    import TestSchema._
    prepareDummyTable((1,1,1),(2,1,2),(3,1,2),(4,2,1),(5,3,1))
    //Session.currentSession.setLogger(System.err.println(_))

    val q0 = from(dummy)(d => where(LogicalBoolean.and(Seq()))
                               select(d)).toList
    q0 should have length(5)
    
    val q1 = from(dummy)(d => where(LogicalBoolean.and(Seq(d.id===1)))
                              select(d)).toList
    q1 should have length(1)
    q1.head.id should equal(1)

    val a2 = (d:Dummy) => LogicalBoolean.and(Seq(d.p1 === 1, d.p2===2))
    val q2 = from(dummy)(d => where(a2(d))select(d)).toList
    q2 should have length(2)

    val a3 = (d:Dummy) => LogicalBoolean.and(Seq(d.p1 === 1,
                                                  d.p2===2, 
                                                  d.id===2))
    val q3 = from(dummy)(d => where(a3(d))select(d)).toList
    q3 should have length(1)
  }

  test("or operation") {
    import TestSchema._
    prepareDummyTable((1,1,1),(2,1,2),(3,1,2),(4,2,1),(5,3,1))

    //Session.currentSession.setLogger(System.err.println(_))

    val q1 = from(dummy)(d => where(LogicalBoolean.or(Seq()))
                               select(d)).toList

    q1 should have length(0)
  }


  test("TrueLogicalBoolean, FalseLogicalBoolean") {
    import TestSchema._
    prepareDummyTable((1, 1, 1), (2, 1, 2))

    // Session.currentSession.setLogger(System.err.println(_))

    from(dummy)(d => where(TrueLogicalBoolean) select (d)).
      size should equal(2)

    from(dummy)(d => where(TrueLogicalBoolean and d.p2 === 1) select (d)).
      size should equal(1)

    from(dummy)(d => where(TrueLogicalBoolean or d.p2 === 1) select (d)).
      size should equal(2)

    from(dummy)(d => where(FalseLogicalBoolean) select (d)).
      size should equal(0)

    from(dummy)(d => where(FalseLogicalBoolean and d.p2 === 1) select (d)).
      size should equal(0)

    from(dummy)(d => where(FalseLogicalBoolean or d.p2 === 1) select (d)).
      size should equal(1)
  }

  test("and/or operators for Option[LogicalBoolean]") {
    import TestSchema._
    prepareDummyTable((1, 1, 1), (2, 1, 2))
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    // Session.currentSession.setLogger(System.err.println(_))

    def q1(opt: Option[Int]) = from(dummy)(d => where(TrueLogicalBoolean and opt.map(_ === d.p2)) select (d))
    q1(none).size should equal(2)
    q1(some).size should equal(1)

    def q2(opt: Option[Int]) = from(dummy)(d => where(FalseLogicalBoolean or opt.map(_ === d.p2)) select (d))
    q2(none).size should equal(0)
    q2(some).size should equal(1)

    def q3(opt: Option[Int]) = from(dummy)(d => where(FalseLogicalBoolean and opt.map(_ === d.p2)) select (d))
    q3(none).size should equal(0)
    q3(some).size should equal(0)

  }

  def prepareDummyTable(vals: (Int, Int, Int)*) {
    for (v <- vals) TestSchema.dummy.insert(new Dummy(v._1, v._2, v._3))
  }

}
