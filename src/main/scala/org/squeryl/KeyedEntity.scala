package org.squeryl


/**
 *  For use with View[A] or Table[A], when A extends KeyedEntity[K],
 * lookup and delete by key become implicitely available
 * Example :
 *
 * class Peanut(weight: Float) extends KeyedEntity[Long]
 * val peanutJar = Table[Peanut]
 *
 * Since Peanut extends KeyedEntity the delete(l:Long)
 * method is available
 *  
 * def removePeanut(idOfThePeanut: Long) =
 *   peanutJar.delete(idOfThePeanut)
 *
 * And lookup by id is also implicitely available :
 * 
 * peanutJar.lookup(idOfThePeanut)
 *
 */

trait KeyedEntity[K] {

  def id: K
}

trait CompositeKeyedEntity[K <: Product] extends KeyedEntity[K]
