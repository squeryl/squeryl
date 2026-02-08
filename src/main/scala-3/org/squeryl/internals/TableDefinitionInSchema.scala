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
package org.squeryl.internals

import org.squeryl.*
import reflect.ClassTag

trait TableDefinitionInSchema {
  self: Schema =>

  protected inline def table[T]()(implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]): Table[T] =
    val optionalFieldsInfo = org.squeryl.internals.TypeInfo.fieldsInfo[T]
    table(tableNameFromClass(manifestT.runtimeClass))(optionalFieldsInfo)(manifestT, ked)

  protected def table[T](
    optionalFieldsInfo: Map[String, Class[_]]
  )(implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]): Table[T] =
    table(tableNameFromClass(manifestT.runtimeClass))(optionalFieldsInfo)(manifestT, ked)

  protected inline def table[T](name: String, prefix: String)(implicit
    manifestT: ClassTag[T],
    ked: OptionalKeyedEntityDef[T, ?]
  ): Table[T] = {
    val optionalFieldsInfo = org.squeryl.internals.TypeInfo.fieldsInfo[T]
    table(name, prefix)(optionalFieldsInfo)(manifestT, ked)
  }

  protected def table[T](name: String, prefix: String)(
    optionalFieldsInfo: Map[String, Class[_]]
  )(implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]): Table[T] = {
    val typeT = manifestT.runtimeClass.asInstanceOf[Class[T]]
    val t = new Table[T](name, typeT, this, Some(prefix), ked.keyedEntityDef, None)
    _addTable(t)
    _addTableType(typeT, t)
    t
  }

  protected inline def table[T](
    name: String
  )(implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]): Table[T] =
    val optionalFieldsInfo = org.squeryl.internals.TypeInfo.fieldsInfo[T]
    table(name)(optionalFieldsInfo)(manifestT, ked)

  protected def table[T](name: String)(
    optionalFieldsInfo: Map[String, Class[_]]
  )(implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]): Table[T] = {
    val typeT = manifestT.runtimeClass.asInstanceOf[Class[T]]
    val t = new Table[T](name, typeT, this, None, ked.keyedEntityDef, Some(optionalFieldsInfo))
    _addTable(t)
    _addTableType(typeT, t)
    t
  }

}
