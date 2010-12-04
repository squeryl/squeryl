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
 ***************************************************************************** */
package org.squeryl.dsl

import org.squeryl.{ForeignKeyDeclaration, Table, Query, KeyedEntity}
import collection.mutable.{HashMap, ArrayBuffer, Buffer}

trait Relation[L <: KeyedEntity[_],R] {
  
  def leftTable: Table[L]

  def rightTable: Table[R]
}

trait OneToManyRelation[O <: KeyedEntity[_],M] extends Relation[O,M] {

  def foreignKeyDeclaration: ForeignKeyDeclaration

  @deprecated("Use foreignKeyDeclaration instead")
  final def foreingKeyDeclaration = foreignKeyDeclaration
  
  def left(leftSide: O): OneToMany[M]

  def leftStateful(leftSide: O) = new StatefulOneToMany[M](left(leftSide))

  def right(rightSide: M): ManyToOne[O]

  def rightStateful(rightSide: M) = new StatefulManyToOne[O](right(rightSide))
}

class StatefulOneToMany[M](val relation: OneToMany[M]) extends Iterable[M] {

  private val _buffer = new ArrayBuffer[M]

  refresh
  
  def refresh = {
    _buffer.clear
    for(m <- relation)
      _buffer.append(m)
  }

  def iterator = _buffer.iterator

  def associate(m: M)(implicit ev: M <:< KeyedEntity[_]) = {
    relation.associate(m)
    _buffer.append(m)
  }

  def deleteAll: Int = {
    val r = relation.deleteAll
    _buffer.clear
    r
  }
}

class StatefulManyToOne[O <: KeyedEntity[_]](val relation: ManyToOne[O]) {

  private var _one: Option[O] = None

  refresh

  def refresh = 
    _one = relation.headOption

  def one = _one

  def assign(o: O) = {
    relation.assign(o)
    _one = Some(o)
  }

  def delete = {
    val b = relation.delete
    _one = None
    b
  }
}

trait ManyToManyRelation[L <: KeyedEntity[_], R <: KeyedEntity[_], A <: KeyedEntity[_]] extends Relation[L,R] {
  self: Table[A] =>

  def thisTable: Table[A]

  def leftForeignKeyDeclaration: ForeignKeyDeclaration

  @deprecated("Use leftForeignKeyDeclaration instead")
  final def leftForeingKeyDeclaration = leftForeignKeyDeclaration

  def rightForeignKeyDeclaration: ForeignKeyDeclaration
  
  @deprecated("Use rightForeignKeyDeclaration instead")
  final def rightForeingKeyDeclaration = rightForeignKeyDeclaration

  def left(leftSide: L): ManyToMany[R,A]

  def leftStateful(leftSide: L) = new StatefulManyToMany[R,A](left(leftSide))

  def right(rightSide: R): ManyToMany[L,A]

  def rightStateful(rightSide: R) = new StatefulManyToMany[L,A](right(rightSide))
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
trait ManyToMany[O <: KeyedEntity[_],A <: KeyedEntity[_]] extends Query[O] {

  /**
   * @param a: the association object
   * 
   * Sets the foreign keys of the association object to the primary keys of the left and right side,
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
   * foreign keys in the relations
   *  
   */
  def associate(o: O): A

  /**
   * Causes the deletion of the 'Association object' between this side and the other side
   * of the relation.
   * @return true if 'o' was associated (if an association object existed between 'this' and 'o') false otherwise
   */

  def dissociate(o: O): Boolean

  /**
   *  Deletes all "associations" relating this "side" to the other
   */
  def dissociateAll: Int

  /**
   * a Query returning all of this member's association entries 
   */
  def associations: Query[A]

  /**
   * @return a Query of Tuple2 containing all objects on the 'other side' along with their association object
   */
  def associationMap: Query[(O,A)]
}


class StatefulManyToMany[O <: KeyedEntity[_],A <: KeyedEntity[_]](val relation: ManyToMany[O,A]) extends Iterable[O] {

  private val _map = new HashMap[O,A]

  refresh

  def refresh = {
    _map.clear
    for(e <- relation.associationMap)
      _map.put(e._1, e._2)
  }

  def iterator = _map.keysIterator

  def associate(o: O, a: A) = {
    relation.associate(o, a)
    _map.put(o, a)
  }

  def associate(o: O): A = {
    val a = relation.associate(o)
    _map.put(o, a)
    a
  }

  def dissociate(o: O): Boolean = {
    val b1 = relation.dissociate(o)
    val b2 = _map.remove(o) != None
    assert(b1 == b2,
      'MutableManyToMany + " out of sync " + o.getClass.getName +" with id=" +
      o.id + (if(b1) "" else "does not") + " exist in the db, and cached collection says the opposite")
    b1
  }

  def dissociateAll: Int = {
    val r = relation.dissociateAll
    _map.clear
    r
  }

  def associations: Iterable[A] =
    _map.values.toSeq  
}


trait OneToMany[M] extends Query[M] {

  /**
   * @param the object on the 'many side' to be associated with this
   *
   *  Sets the foreign key of 'm' to refer to the primary key of the 'one' instance
   */
  def assign(m: M): Unit

  /**
   * Calls 'assign(m)' and persists the changes the database, by inserting or updating 'm', depending
   * on if it has been persisted or not.
   */
  def associate(m: M)(implicit ev: M <:< KeyedEntity[_]): Unit
  
  def deleteAll: Int
}

trait ManyToOne[O <: KeyedEntity[_]] extends Query[O] {

  def assign(one: O): Unit

  def delete: Boolean
}
