package org.squeryl.tests

import org.squeryl
import squeryl._
import squeryl.PrimitiveTypeMode._
import squeryl.dsl.ast._

import org.specs._
import java.util.UUID

object UuidTests {
  class UuidAsProperty extends KeyedEntity[Long] {
    val id: Long = 0
    val uuid = UUID.randomUUID
  }

  class UuidAsId extends KeyedEntity[UUID] {
    val id = UUID.randomUUID
    lazy val foreigns = TestSchema.uuidOneToMany.left(this)
  }

  class UuidAsForeignKey(val foreignUuid: UUID) extends KeyedEntity[Long] {
    val id: Long = 0
  }

  object TestSchema extends Schema {
    val uuidAsProperty = table[UuidAsProperty]
    val uuidAsId = table[UuidAsId]
    val uuidAsForeignKey = table[UuidAsForeignKey]

    val uuidOneToMany = oneToManyRelation(uuidAsId, uuidAsForeignKey).via(_.id === _.foreignUuid)

    override def drop = {
      Session.cleanupResources
      super.drop
    }
  }

}

class UuidTests extends QueryTester {

  import UuidTests._

  def prepare() = {
    try {
      TestSchema.drop
    }
    catch {
      case e:Exception => {}
    }

    TestSchema.create
  }

  def cleanup() = TestSchema.drop
  def dumpSchema() = TestSchema.printDdl

  def testUuidAsProperty() {
    import TestSchema._

    val testObject = new UuidAsProperty
    uuidAsProperty.insert(testObject)

    assertEquals(testObject.uuid, uuidAsProperty.where(_.id === testObject.id).single.uuid, 'testUuidAsProperty)

    passed('testUuidAsProperty)
  }

  def testUuidAsId() {
    import TestSchema._

    val testObject = new UuidAsId
    uuidAsId.insert(testObject)

    assertEquals(testObject.id, uuidAsId.where(_.id === testObject.id).single.id, 'testUuidAsId)

    passed('testUuidAsId)
  }

  def testUuidAsForeignKey() {
    import TestSchema._

    val primaryObject = new UuidAsId
    uuidAsId.insert(primaryObject)

    val secondaryObject = new UuidAsForeignKey(primaryObject.id)
    uuidAsForeignKey.insert(secondaryObject)

    assertEquals(secondaryObject.id, uuidAsForeignKey.where(_.id === secondaryObject.id).single.id, 'testUuidAsForeignKey)

    assertEquals(List(secondaryObject.id), primaryObject.foreigns.map(_.id).toList, 'testUuidAsForeignKey)

    passed('testUuidAsForeignKey)
  }

  def testAll() {
    prepare()

    testUuidAsProperty()
    testUuidAsId()
    testUuidAsForeignKey()

    cleanup()
  }

}
