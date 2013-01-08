package org.squeryl.test

import org.squeryl._
import org.squeryl.framework.{SchemaTester, RunTestsInsideTransaction}
import java.util.UUID
import org.squeryl.test.PrimitiveTypeModeForTests._
import framework.SingleTestRun

object UuidTests {
  class UuidAsProperty extends KeyedEntity[Long] {
    val id: Long = 0
    val uuid = UUID.randomUUID
  }
  
  class UuidWithOption(val optionalUuid: Option[UUID]) extends KeyedEntity[Long] {
    def this() = this(Some(UUID.randomUUID()))
    val id: Long = 0
  }  
  
  class UuidAsId extends KeyedEntity[UUID] {
    var id = UUID.randomUUID
    lazy val foreigns = TestSchema.uuidOneToMany.left(this)
  }

  class UuidAsForeignKey(val foreignUuid: UUID) extends KeyedEntity[Long] {
    val id: Long = 0
  }

  object TestSchema extends Schema {
    val uuidAsProperty = table[UuidAsProperty]
    val uuidAsId = table[UuidAsId]
    val uuidAsForeignKey = table[UuidAsForeignKey]
    val uuidWithOption = table[UuidWithOption]

    val uuidOneToMany = oneToManyRelation(uuidAsId, uuidAsForeignKey).via(_.id === _.foreignUuid)

    override def drop = {
      Session.cleanupResources
      super.drop
    }
  }

}

abstract class UuidTests extends SchemaTester with RunTestsInsideTransaction{
  import UuidTests._

  final def schema = TestSchema

  test("UuidAsProperty") {
    import TestSchema._

    val testObject = new UuidAsProperty
    testObject.save

    testObject.uuid should equal(uuidAsProperty.where(_.id === testObject.id).single.uuid)

    testObject.uuid should equal(uuidAsProperty.where(_.uuid in List(testObject.uuid)).single.uuid)
  }

  test("UuidOptional", SingleTestRun) {
    import TestSchema._

    val testObject = new UuidWithOption(None)
    testObject.save
    
    val fromDb = uuidWithOption.lookup(testObject.id).get
    println(fromDb.optionalUuid)
    fromDb.optionalUuid should equal(None)
    
    val uuid = UUID.randomUUID()
    
    update(uuidWithOption)(p =>
      where(p.id === testObject.id)
      set(p.optionalUuid := Some(uuid))
    )
    
    uuidWithOption.lookup(testObject.id).get.optionalUuid should equal(Some(uuid))

    update(uuidWithOption)(p =>
      where(p.id === testObject.id)
      set(p.optionalUuid := None)
    )
    
    uuidWithOption.lookup(testObject.id).get.optionalUuid should equal(None)    
  }
  
  test("UuidAsId") {
    import TestSchema._

    val testObject = new UuidAsId

    testObject.save

    testObject.id should equal(uuidAsId.where(_.id === testObject.id).single.id)

    testObject.id should equal(uuidAsId.where(_.id in List(testObject.id)).single.id)

    val lookup = uuidAsId.lookup(testObject.id)
    lookup.get.id should equal(testObject.id)
  }

  test("UuidAsForeignKey") {
    import TestSchema._

    val primaryObject = new UuidAsId
    primaryObject.save

    val secondaryObject = new UuidAsForeignKey(primaryObject.id)
    uuidAsForeignKey.insert(secondaryObject)

    secondaryObject.id should equal(uuidAsForeignKey.where(_.id === secondaryObject.id).single.id)

    List(secondaryObject.id) should equal(primaryObject.foreigns.map(_.id).toList)
  }
}