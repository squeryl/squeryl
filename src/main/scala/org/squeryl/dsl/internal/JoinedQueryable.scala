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
package org.squeryl.dsl.internal

import org.squeryl.Queryable
import org.squeryl.internals.ResultSetMapper
import java.sql.ResultSet

trait JoinedQueryable[A] extends Queryable[A] {
  
  def name =
    throw new UnsupportedOperationException('OuterJoinedQueryable + " is a temporary class, not meant to become part of the ast")

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) =
    throw new UnsupportedOperationException('OuterJoinedQueryable + " is a temporary class, not meant to become part of the ast")
}

class OuterJoinedQueryable[A](val queryable: Queryable[A], val leftRightOrFull: String) extends JoinedQueryable[Option[A]]{
  
  /**
   * Allowing an implicit conversion from OuterJoinedQueryable to OptionalQueryable will trigger another conversion
   * to InnerJoinedQueryable inside org.squeryl.dsl.boilerplate.JoinSignatures#join.  This also allows us to inhibit
   * the table without using Option[Option[T]] in our results 
   */
  def inhibitWhen(inhibited: Boolean) = {
    this.inhibited = inhibited
    this
  }
  
}

class InnerJoinedQueryable[A](val queryable: Queryable[A], val leftRightOrFull: String) extends JoinedQueryable[A]
