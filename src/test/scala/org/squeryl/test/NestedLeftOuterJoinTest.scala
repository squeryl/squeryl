package org.squeryl.test

import org.squeryl._
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl.framework._

object TestSchema extends Schema {
  val a = table[A]
  val b = table[B]

  override def drop = super.drop
}

class A(val id: Int, val name: String) extends KeyedEntity[Int]

class B(val id: Int, val name: String, val aId: Int) extends KeyedEntity[Int]

abstract class NestedLeftOuterJoinTest extends SchemaTester with RunTestsInsideTransaction{

  def schema = TestSchema

  def testInnerJoin() = {
    val q0 = from(TestSchema.b)( b => select(b) )

    val q1 = from(TestSchema.a, q0) ( (a, b) =>
      where(a.id === b.aId)
      select(a, b)
    )

    checkJoinQuery(q1)

    val q2 =
      join(TestSchema.a, q0) ( (a, b) =>
        select(a, b)
          on(a.id === b.aId)
      )

    checkJoinQuery(q2)
  }

  test("InnerJoin"){

    TestSchema.a.insert(new A(1, "a one"))

    TestSchema.b.insert(new B(1, "b one", 1))

    testInnerJoin


    val q0 = from(TestSchema.b)( b => select(b) )

    val q1 = from(TestSchema.a, q0) ( (a, b) =>
      where(a.id === b.aId)
      select(a, b)
    )

    checkJoinQuery(q1)

    val aQuery = join(TestSchema.a, q0.leftOuter) ( (a, b) =>
      select(a, b)
        on(a.id === b.map(_.aId))
    )

    checkLeftJoinQuery(aQuery)
  }

  def checkLeftJoinQuery(q: Query[(A, Option[B])]) {
    q.headOption.map { (result) =>
      val (a, b) = result

      b should not equal(None)
    }
  }

  def checkJoinQuery(q: Query[(A, B)]) {
    q.headOption should not equal(None)
  }


}
