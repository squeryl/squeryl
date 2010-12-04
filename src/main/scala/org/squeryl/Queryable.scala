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

import dsl.ast.{LogicalBoolean}
import dsl.{QueryDsl}
import internals.ResultSetMapper
import java.sql.ResultSet

trait Queryable[T] {
  
  def name: String

  private[squeryl] var inhibited = false

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) : T

  def where(whereClauseFunctor: T => LogicalBoolean)(implicit dsl: QueryDsl): Query[T] = {
    import dsl._
    from(this)(q0 =>
      dsl.where(whereClauseFunctor(q0))
      select(q0)
    )
  }
}
