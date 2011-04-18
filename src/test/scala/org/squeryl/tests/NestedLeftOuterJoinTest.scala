package org.squeryl.tests

import org.squeryl.adapters.H2Adapter
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object NestedLeftOuterJoinTest {
  def main(args: Array[String]) {
    SessionFactory.concreteFactory = Some(createH2TestConnection _)

    inTransaction {
      doIt
    }
  }

  def testInnerJoin = {
    import TestSchema._
    val q0 = from(b)( b => select(b) )

    val q1 = from(a, q0) ( (a, b) =>
      where(a.id === b.aId)
      select(a, b)
    )

    checkJoinQuery(q1)

    val q2 =
      join(a, q0) ( (a, b) =>
        select(a, b)
          on(a.id === b.aId)
      )

    checkJoinQuery(q2)
  }

  def doIt ={
    TestSchema.drop

    TestSchema.create

    import TestSchema._
    a.insert(new A(1, "a one"))

    b.insert(new B(1, "b one", 1))

    testInnerJoin


    val q0 = from(b)( b => select(b) )

    val q1 = from(a, q0) ( (a, b) =>
      where(a.id === b.aId)
      select(a, b)
    )

    checkJoinQuery(q1)

    val aQuery = join(a, q0.leftOuter) ( (a, b) =>
      select(a, b)
        on(a.id === b.map(_.aId))
    )

    checkLeftJoinQuery(aQuery)
  }

  def checkLeftJoinQuery(q: Query[(A, Option[B])]) {
    q.headOption.map { (result) =>
      val (a, b) = result

      b match {
        case None => error("darn... no go")
        case Some(_) => println("good to go")
      }
    }
  }

  def checkJoinQuery(q: Query[(A, B)]) {
    q.headOption match {
      case None => error("darn... no go")
      case Some(_) => println("good to go")
    }
  }

  def createH2TestConnection(): Session = {
    Class.forName("org.h2.Driver");

    val session = new Session(
      java.sql.DriverManager.getConnection("jdbc:h2:~/test", "sa", ""),
      new H2Adapter,
      None
    )

    session
  }

  object TestSchema extends Schema {
    val a = table[A]
    val b = table[B]

    override def drop = super.drop
  }

  class A(val id: Int, val name: String) extends KeyedEntity[Int]

  class B(val id: Int, val name: String, val aId: Int) extends KeyedEntity[Int]
}
