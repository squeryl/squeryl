/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package org.squeryl.dsl

import org.squeryl.{ForeingKeyDeclaration, Table, Query, KeyedEntity}

trait Relation[L <: KeyedEntity[_],R <: KeyedEntity[_]] {
  
  def leftTable: Table[L]

  def rightTable: Table[R]
}

trait OneToManyRelation[O <: KeyedEntity[_],M <: KeyedEntity[_]] extends Relation[O,M] {

  def foreingKeyDeclaration: ForeingKeyDeclaration
  
  def left(leftSide: O): OneToMany[M]

  def right(rightSide: M): ManyToOne[O]
}

trait ManyToManyRelation[L <: KeyedEntity[_], R <: KeyedEntity[_], A <: KeyedEntity[_]] extends Relation[L,R] {
  self: Table[A] =>

  def thisTable: Table[A]

  def leftForeingKeyDeclaration: ForeingKeyDeclaration

  def rightForeingKeyDeclaration: ForeingKeyDeclaration

  def left(leftSide: L): ManyToMany[R,A]
  
  def right(rightSide: R): ManyToMany[L,A]
}


/**
 * This trait is what is referred by both the left and right side of a manyToMany relation.
 * Type parameters are :
 *   O: the type at the "other" side of the relation
 *   A: the association type i.e. the entity in the "middle" of the relation
 *
 *  Object mapping to the "middle" entity are called "association objects"
 *
 * this trait extends Query[O] and can be queried against like a normal query.
 *
 * Note that this trait is used on both "left" and "right" sides of the relation,
 * but in a given relation  
 */
trait ManyToMany[O,A <: KeyedEntity[_]] extends Query[O] {

  /**
   * @param a: the association object
   * 
   * Sets the foreing keys of the association object to the primary keys of the left and right side,
   * this method does not update the database, changes to the association object must be done for
   * the operation to be persisted. Alternatively the method 'associate(o, a)' will call this assign(o, a)
   * and persist the changes.  
   */
  def assign(o: O, a: A): Unit

  /**
   * @param a: the association object
   *
   * Calls assign(o,a) and persists the changes the database, by inserting or updating 'a', depending
   * on if it has been persisted or not.
   */
  def associate(o: O, a: A): Unit

  /**
   * Creates a new association object 'a' and calls assign(o,a)
   */
  def assign(o: O): A

  /**
   * Creates a new association object 'a' and calls associate(o,a)
   *
   * Note that this method will fail if the association object has NOT NULL constraint fields appart from the
   * foreing keys in the relations
   *  
   */
  def associate(o: O): A

  /**
   *  Deletes all "associations" relating this "side" to the other
   */
  def dissociateAll: Int

  /**
   * a Query returning all of this member's association entries 
   */
  def associations: Query[A]
}


trait OneToMany[M] extends Query[M] {

  /**
   * @param the object on the 'many side' to be associated with this
   *
   *  Sets the foreing key of 'm' to refer to the primary key of the 'one' instance
   */
  def assign(m: M): Unit

  /**
   * Calls 'assign(m)' and persists the changes the database, by inserting or updating 'm', depending
   * on if it has been persisted or not.
   */
  def associate(m: M): Unit
  
  def deleteAll: Int
}

trait ManyToOne[O] extends Query[O] {

  def assign(one: O): Unit

  def delete: Unit
}
