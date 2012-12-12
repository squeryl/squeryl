package org.squeryl.test.arrays

import _root_.org.squeryl.framework._

abstract class PrimitiveArrayTest extends SchemaTester with RunTestsInsideTransaction {

  import _root_.org.squeryl.PrimitiveTypeMode._

  val schema = PrimitiveArraySchema

  import PrimitiveArraySchema._

  test("can insert and query integer array values in database") {
    transaction {
      schema.drop
      schema.create
      swimmers.insert(new Swimmer(1, Array(1055, 1299, 1532)))
    }
    
    val query = from(swimmers)((s) => select(s))
    println(query.toString)
    val res = transaction { query.toList }

    res.size should equal(1)
    res(0).lap_times.size should equal(3)
    res(0).lap_times(0) should equal(1055)
    res(0).lap_times(1) should equal(1299)
    res(0).lap_times(2) should equal(1532)
  }
}

import _root_.org.squeryl.Schema
import _root_.org.squeryl.PrimitiveTypeMode._

object PrimitiveArraySchema extends Schema {

  val swimmers = table[Swimmer]("swimmer")

  override def drop = super.drop
}

class Swimmer(val id: Int, val lap_times: Array[Int])