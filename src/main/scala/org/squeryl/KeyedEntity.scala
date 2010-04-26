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
package org.squeryl

import annotations.Transient


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

trait KeyedEntity[K] extends PersistenceStatus {

  def id: K
}

trait PersistenceStatus {
  
  @Transient
  private [squeryl] var _isPersisted = false

  def isPersisted: Boolean = _isPersisted
}

trait CompositeKeyedEntity[K <: Product] extends KeyedEntity[K]


trait Optimistic {
  self: KeyedEntity[_] =>

  protected val occVersionNumber = 0
}

class StaleUpdateException(message: String) extends RuntimeException(message)

trait EntityMember {

  def entityRoot[B]: Query[B]
}
