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


package object annotations {
  /**
   *
   * Please note: Squeryl 0.9.5 supports Scala 2.9 -> 2.11.
   * Before 0.9.5-7 the @Column attribute was defined in a way so that it
   * would automatically be applied to the Java @field of a val or var.
   * The Scala teams choice to change the location of the @field annotation
   * unfortunately made this behavior impossible
   * to retain.  If you are using the @Column attribute ito annotate
   * constructor parameters you MUST define it as @(Column @field).  Better yet, don't use
   * the Column annotation at all and define your column meta data in the Schema instead.
   *
   * Squeryl has default mappings for all Java primitive types.
   * Scala/Java
   * Int/int  -> 4 byte number
   * Long/long  -> 8 byte number
   * Float/float -> 4 byte floating point
   * String -> varchar(256)
   *
   * The default mappings can be overridden at the field/column level using the
   * Column attribute, and they can also be  overridden at the Schema level
   * by overriding the method. For example, the following causes all string
   * field in the schema to become varchars of length 64 :
   *
   * override def columnTypeFor(fieldMetaData: FieldMetaData, databaseAdapter: DatabaseAdapter) =
   *   if(fieldMetaData.isStringType)
   *     return "varchar(64)"
   *   else
   *     super.columnTypeFor(fieldMetaData, databaseAdapter)
   */
  type Column = ColumnBase
}