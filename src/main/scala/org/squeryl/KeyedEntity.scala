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
package org.squeryl

import annotations.Transient
import java.sql.SQLException

@scala.annotation.implicitNotFound(msg = "The method requires an implicit org.squeryl.KeyedEntityDef[${A}, ${K}] in scope, or that it extends the trait KeyedEntity[${K}]")
trait KeyedEntityDef[-A,K] extends OptionalKeyedEntityDef[A,K]{
  
  def getId(a: A): K
  /**
   * returns true if the given instance has been persisted
   */
  def isPersisted(a: A):  Boolean
  /**
   * the (Scala) property/field name of the id 
   */
  def idPropertyName: String
  /**
   * the counter field name for OCC, None to disable OCC (optimistic concurrency control)
   */
  def optimisticCounterPropertyName: Option[String] = None
  
  private [squeryl] def isOptimistic = optimisticCounterPropertyName.isDefined
  
  /**
   * fulfills the contract of OptionalKeyedEntityDef
   */
  final def keyedEntityDef = Some(this)
}

trait OptionalKeyedEntityDef[-A,K] {
  def keyedEntityDef: Option[KeyedEntityDef[A,K]]
}

/**
 *  For use with View[A] or Table[A], when A extends KeyedEntity[K],
 * lookup and delete by key become implicitly available
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
 * And lookup by id is also implicitly available :
 * 
 * peanutJar.lookup(idOfThePeanut)
 *
 */

trait KeyedEntity[K] extends PersistenceStatus {

  def id: K

  override def hashCode =
    if(isPersisted)
      id.hashCode
    else
      super.hashCode

  override def equals(z: Any):Boolean = {
    if(z == null)
      return false
    val ar = z.asInstanceOf[AnyRef]
    if(!ar.getClass.isAssignableFrom(this.getClass))
      false
    else if(isPersisted)
      id == ar.asInstanceOf[KeyedEntity[K]].id
    else
      super.equals(z)
  }
}


trait PersistenceStatus {

  @transient
  @Transient
  private [squeryl] var _isPersisted = false

  def isPersisted: Boolean = _isPersisted
}

trait IndirectKeyedEntity[K,T] extends KeyedEntity[K] {
  
  def idField: T
}


trait Optimistic {
  self: KeyedEntity[_] =>

  protected val occVersionNumber = 0
}

/** Thrown to indicate that an error has occurred in the SQL database */
object SquerylSQLException {
  def apply(message: String, cause: SQLException) =
    new SquerylSQLException(message, Some(cause))
  def apply(message: String) =
    new SquerylSQLException(message, None)
}

class SquerylSQLException(message: String, cause: Option[SQLException]) extends RuntimeException(message, cause.orNull) {
  // Overridden to provide covariant return type as a convenience
  override def getCause: SQLException = cause.orNull
}

class StaleUpdateException(message: String) extends RuntimeException(message)

trait EntityMember {

  def entityRoot[B]: Query[B]
}


trait ReferentialAction {
  def event: String
  def action: String
}

/**
 * ForeignKeyDeclaration are to be manipulated only during the Schema definition
 * (this is why all public methods have the implicit arg (implicit ev: Schema))
 */
class ForeignKeyDeclaration(val idWithinSchema: Int, val foreignKeyColumnName: String, val referencedPrimaryKey: String) {

  private var _referentialActions: Option[(Option[ReferentialAction],Option[ReferentialAction])] = None

  private [squeryl] def _isActive =
    _referentialActions != None

  private [squeryl] def _referentialAction1: Option[ReferentialAction] =
    _referentialActions.get._1

  private [squeryl] def _referentialAction2: Option[ReferentialAction] =
    _referentialActions.get._2

  /**
   * Causes the foreign key to have no constraint 
   */
  def unConstrainReference()(implicit ev: Schema) =
    _referentialActions = None

  /**
   * Will cause a foreign key constraint to be created at schema creation time :
   * alter table <tableOfForeignKey> add foreign key (<foreignKey>) references <tableOfPrimaryKey>(<primaryKey>)
   */
  def constrainReference()(implicit ev: Schema) =
    _referentialActions = Some((None, None))

  /**
   * Does the same as constrainReference, plus adds a ReferentialAction (ex.: foreignKeyDeclaration.constrainReference(onDelete cascade)) 
   */
  def constrainReference(a1: ReferentialAction)(implicit ev: Schema) =
    _referentialActions = Some((Some(a1), None))

  /**
   * Does the same as constrainReference, plus adds two ReferentialActions
   * (ex.: foreignKeyDeclaration.constrainReference(onDelete cascade, onUpdate restrict)) 
   */
  def constrainReference(a1: ReferentialAction, a2: ReferentialAction)(implicit ev: Schema) =
    _referentialActions = Some((Some(a1), Some(a2)))
}
