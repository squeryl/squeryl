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

package org.squeryl.internals

import java.lang.reflect.Method
import org.squeryl.Schema


object PosoLifeCycleEvent extends Enumeration {
  type PosoLifeCycleEvent = Value
  val  BeforeInsert,AfterInsert,
       BeforeDelete,AfterDelete,
       BeforeUpdate,AfterUpdate,
       AfterSelect = Value
}

class LifecycleEventInvoker(val event: PosoLifeCycleEvent.Value, val posoClass: Class[_], val method: Method)


trait PosoLifecycleEventListener {
  def beforeInsert(a:AnyRef):Unit
  def afterInsert(a:AnyRef):Unit
  def beforeDelete(a:AnyRef):Unit
  def afterDelete(a:AnyRef):Unit
  def beforeUpdate(a:AnyRef):Unit
  def afterUpdate(a:AnyRef):Unit
  def afterSelect(a:AnyRef):Unit
}

object NoOpPosoLifecycleEventListener extends PosoLifecycleEventListener {
  def beforeInsert(a:AnyRef) = {}
  def afterInsert(a:AnyRef) = {}
  def beforeDelete(a:AnyRef) = {}
  def afterDelete(a:AnyRef) = {}
  def beforeUpdate(a:AnyRef) = {}
  def afterUpdate(a:AnyRef) = {}
  def afterSelect(a:AnyRef) = {}
}

class PosoLifecycleEventListenerImpl(
  schema: Schema,
  beforeInsertInvoker: Option[LifecycleEventInvoker],
  afterInsertInvoker: Option[LifecycleEventInvoker],
  beforeDeleteInvoker: Option[LifecycleEventInvoker],
  afterDeleteInvoker: Option[LifecycleEventInvoker],
  beforeUpdateInvoker: Option[LifecycleEventInvoker],
  afterUpdateInvoker: Option[LifecycleEventInvoker],
  afterSelectInvoker: Option[LifecycleEventInvoker]) extends PosoLifecycleEventListener {

  private def _invokeIfNotNone(i: Option[LifecycleEventInvoker], a:AnyRef) =
    if(i != None) {
      i.get.method.invoke(schema, a)
    }

  def beforeInsert(a:AnyRef) =
    _invokeIfNotNone(beforeInsertInvoker, a)

  def afterInsert(a:AnyRef) =
    _invokeIfNotNone(afterInsertInvoker, a)

  def beforeDelete(a:AnyRef) =
    _invokeIfNotNone(beforeDeleteInvoker, a)

  def afterDelete(a:AnyRef) =
    _invokeIfNotNone(afterDeleteInvoker, a)

  def beforeUpdate(a:AnyRef) =
    _invokeIfNotNone(beforeUpdateInvoker, a)

  def afterUpdate(a:AnyRef) =
    _invokeIfNotNone(afterUpdateInvoker, a)
  
  def afterSelect(a:AnyRef) =
    _invokeIfNotNone(afterSelectInvoker, a)
}