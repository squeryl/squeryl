package org.squeryl.test.immutable

import org.squeryl.Schema
import org.squeryl.PrimitiveTypeMode._
import org.squeryl._
import org.squeryl.framework.SchemaTester
import org.squeryl.framework.QueryTester
import org.squeryl.framework.RunTestsInsideTransaction

case class Immutable(name: String, id: Long = -1)

object ImmutableSchema extends Schema {
  
  implicit object ImmutableKeyDef extends KeyedEntityDef[Immutable, Long] with GeneratedKeyDef[Immutable, Long] {
    
    override def getId(im: Immutable) = im.id
    
    override def setId(im: Immutable, id: Long) = im.copy(id = id)
    
    override def idPropertyName = "id"
      
    override def isPersisted(im: Immutable) = im.id > 0
    
  }
  
  val immutables = table[Immutable]
  
}

abstract class ImmutableTester extends SchemaTester with QueryTester with RunTestsInsideTransaction {

 import ImmutableSchema._
 
 lazy val schema = ImmutableSchema

 test("Insert Immutable") {
    val im = Immutable("test")
    val im2 = immutables.insert(im)
    im2.id should be > (0l)
    val im3 = from(immutables)(i =>
    	where(i.id === im2.id)
    	select(i)).singleOption
    im3 should not equal None
    im3 map { _ should equal (im2) }
  }

}
