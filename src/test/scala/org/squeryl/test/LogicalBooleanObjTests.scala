package org.squeryl.test

import org.squeryl._
import org.squeryl.framework.{SchemaTester, RunTestsInsideTransaction}
import org.squeryl.dsl.ast._

import PrimitiveTypeMode._

object LogicalBooleanObjTests {

  class Dummy(val id:Int, val p1:Int, val p2:Int) extends KeyedEntity[Int];

  object TestSchema extends Schema {
    val dummy = table[Dummy]
  }


}

abstract class LogicalBooleanObjTests extends SchemaTester with RunTestsInsideTransaction{
  import LogicalBooleanObjTests._

  final def schema = TestSchema

  test("and operation") {
    import TestSchema._
    prepareDummyTable((1,1,1),(2,1,2),(3,1,2),(4,2,1),(5,3,1));
    Session.currentSession.setLogger(System.err.println(_));

    val q0 = from(dummy)(d => where(LogicalBoolean.and(Seq()))
                               select(d)).toList;
    q0 should have length(5);
    
    val q1 = from(dummy)(d => where(LogicalBoolean.and(Seq(d.id===1)))
                              select(d)).toList;
    q1 should have length(1);
    q1.head.id should equal(1);

    val a2 = ((d:Dummy) => LogicalBoolean.and(Seq(d.p1 === 1, d.p2===2)))
    val q2 = from(dummy)(d => where(a2(d))select(d)).toList;
    q2 should have length(2);

    val a3 = ((d:Dummy) => LogicalBoolean.and(Seq(d.p1 === 1, 
                                                  d.p2===2, 
                                                  d.id===2)))
    val q3 = from(dummy)(d => where(a3(d))select(d)).toList;
    q3 should have length(1);
  }

  test("or operation") {
    import TestSchema._
    prepareDummyTable((1,1,1),(2,1,2),(3,1,2),(4,2,1),(5,3,1));

    //Session.currentSession.setLogger(System.err.println(_));

    val q1 = from(dummy)(d => where(LogicalBoolean.or(Seq()))
                               select(d)).toList;

    q1 should have length(0);
  }


  test("t and f") {
    import TestSchema._
    prepareDummyTable((1,1,1),(2,1,2));

    //Session.currentSession.setLogger(System.err.println(_));

    val t = LogicalBoolean.t;

    val q = from(dummy)(d => where(LogicalBoolean.t)select(d));
    System.err.println("q="+q);
    val qt = q.toList
    qt should have length(2);

    val qf = from(dummy)(d => where(LogicalBoolean.f)select(d)).toList
    qf should have length(0);


  }

  def prepareDummyTable(vals:(Int,Int,Int)*):Unit =
  {
    for(v <- vals) TestSchema.dummy.insert( new Dummy(v._1,v._2,v._3) );
  }

}
