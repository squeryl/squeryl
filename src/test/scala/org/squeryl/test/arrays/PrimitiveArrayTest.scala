package org.squeryl.test.arrays

import _root_.org.squeryl.framework._
import org.squeryl.Schema
import org.squeryl.test.PrimitiveTypeModeForTests._

abstract class PrimitiveArrayTest extends SchemaTester with RunTestsInsideTransaction {
  self: DBConnector =>
  // repeat the import closer to call site to give priority to our `===` operator
  import org.squeryl.test.PrimitiveTypeMode4Tests._

  val schema: Schema = PrimitiveArraySchema

  import PrimitiveArraySchema._

  test("can insert and query integer, double, and long array values in database") {
    transaction {
      schema.drop
      schema.create
      swimmers.insert(
        new Swimmer(
          1,
          Array(10.55, 12.99, 15.32),
          Array(100, 110, 20),
          Array(9876543210L, 123456789L),
          Array("testing", "stuff")
        )
      )
    }

    val query = from(swimmers)((s) => select(s))
    val res = transaction { query.toList }

    res.size should equal(1)
    res(0).lap_times.size should equal(3)
    res(0).lap_times(0) should equal(10.55)
    res(0).lap_times(1) should equal(12.99)
    res(0).lap_times(2) should equal(15.32)

    res(0).scores.size should equal(3)
    res(0).scores(0) should equal(100)
    res(0).scores(1) should equal(110)
    res(0).scores(2) should equal(20)

    res(0).orgids.size should equal(2)
    res(0).orgids(0) should equal(9876543210L)
    res(0).orgids(1) should equal(123456789L)

    res(0).tags.size should equal(2)
    res(0).tags(0) should equal("testing")
    res(0).tags(1) should equal("stuff")
  }
  test("can update integer, double, and long array values in database") {
    transaction {
      schema.drop
      schema.create
      swimmers.insert(
        new Swimmer(
          1,
          Array(10.55, 12.99, 15.32),
          Array(100, 110, 20),
          Array(9876543210L, 123456789L),
          Array("testing", "stuff")
        )
      )
    }

    val query = from(swimmers)((s) => select(s))
    val res = transaction { query.toList }

    res.size should equal(1)
    res(0).lap_times.size should equal(3)
    res(0).scores.size should equal(3)
    res(0).orgids.size should equal(2)
    res(0).tags.size should equal(2)

    transaction {
      update(swimmers)(s =>
        where(s.id === 1).set(
          s.lap_times := Array(11.69),
          s.scores := Array(1, 2, 3, 4, 5),
          s.orgids := Array(13L),
          s.tags := Array("and things")
        )
      )
    }

    from(swimmers)((s) => select(s))
    val res2 = transaction { query.toList }

    res2.size should equal(1)
    res2(0).lap_times.size should equal(1)
    res2(0).scores.size should equal(5)
    res2(0).orgids.size should equal(1)
    res2(0).tags.size should equal(1)

    res2(0).lap_times(0) should equal(11.69)
    res2(0).scores(2) should equal(3)
    res2(0).orgids(0) should equal(13L)
    res2(0).tags(0) should equal("and things")
  }
}

import _root_.org.squeryl.Schema

object PrimitiveArraySchema extends Schema {

  val swimmers = table[Swimmer]("swimmer")

  override def drop = super.drop
}

class Swimmer(
  val id: Int,
  val lap_times: Array[Double],
  val scores: Array[Int],
  val orgids: Array[Long],
  val tags: Array[String]
)
