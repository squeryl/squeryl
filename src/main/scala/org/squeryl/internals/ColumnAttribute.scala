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


trait ColumnAttribute
trait MultipleColumnAttribute

trait AttributeValidOnMultipleColumn extends ColumnAttribute
trait AttributeValidOnNumericalColumn extends ColumnAttribute
trait AttributeValidOnNonNumericalColumn extends ColumnAttribute

case class Unique() extends ColumnAttribute with MultipleColumnAttribute
  with AttributeValidOnNonNumericalColumn
  with AttributeValidOnNumericalColumn
  with AttributeValidOnMultipleColumn

case class AutoIncremented(var nameOfSequence: Option[String]) extends ColumnAttribute
  with AttributeValidOnNumericalColumn {

  override def hashCode = this.getClass.hashCode
  
  override def equals(any: Any) =
    any.isInstanceOf[AutoIncremented]
}

case class Indexed(val nameOfIndex: Option[String]) extends ColumnAttribute with MultipleColumnAttribute
        with AttributeValidOnNonNumericalColumn
        with AttributeValidOnNumericalColumn
        with AttributeValidOnMultipleColumn

case class PrimaryKey() extends ColumnAttribute
        with AttributeValidOnNonNumericalColumn
        with AttributeValidOnNumericalColumn
        with AttributeValidOnMultipleColumn

case class DBType(val declaration: String) extends ColumnAttribute
  with AttributeValidOnNonNumericalColumn
  with AttributeValidOnNumericalColumn