package org.squeryl.dsl

import org.squeryl.Query

/**
 * This trait is what is referred by both the left and right side of a manyToMany relation.
 * Type parameters are :
 *   O: the type at the "other" side of the relation
 *   A: the association type i.e. the entity in the "middle" of the relation
 *
 * this trait extends Query[O] and can be queried against like a normal query 
 */
trait ManyToManyMember[O,A] extends Query[O] {
  
  def associate(o: O, a: A): A

  def associate(o: O): A

  /**
   * Deletes all "associations" relating this "side" to the other
   */
  def dissociateAll: Int

  def associations: Query[A]
}
