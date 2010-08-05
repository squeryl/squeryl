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

import java.sql.{ResultSet, SQLException, Statement}
import org.squeryl.dsl.boilerplate.Query1
import org.squeryl.Queryable
import org.squeryl.dsl.fsm.QueryElements

object Utils {

  /**
   * Will attempt to evaluate a string expression and will catch any exception.
   * For use in circumstances when loggin is needed (i.e. a fatal error has already occured
   * and we need to log as much info as possible (i.e. put as much info as possible in the 'black box').
   * Also used to allow dumping (ex. for logging) a Query AST *before* it is completely built.
   */
  def failSafeString(s: =>String) =
    _failSafeString(s _, "cannot evaluate")

  def failSafeString(s: =>String, valueOnFail: String) =
    _failSafeString(s _, valueOnFail)

  private def _failSafeString(s: ()=>String, valueOnFail: String) =
    try {
      s()
    }
    catch {
      case e:Exception => valueOnFail
    }

  def close(s: Statement) =
    try {s.close}
    catch {case e:SQLException => {}}

  def close(rs: ResultSet) =
    try {rs.close}
    catch {case e:SQLException => {}}


  private class DummyQueryElements
    extends QueryElements {
    override val whereClause = None
  }
  
  private class DummyQuery[A,B](q: Queryable[A],f: A=>B, g: B=>Unit) extends Query1[A,Int](
    q,
    a => {
      val res = f(a);
      g(res)
      (new DummyQueryElements).select(0)
    },
    true)

  /**
   * visitor will get applied on a proxied Sample object of the Queryable[A],
   * this function is used for obtaining AST nodes or metadata from A.
   */
  def mapSampleObject[A,B](q: Queryable[A], visitor: A=>B): B = {
    var b:Option[B] = None
    // if we are currently building an AST, we must save the (last) _lastAccessedFieldReference
    val prev = FieldReferenceLinker._lastAccessedFieldReference
    new DummyQuery(q, visitor, (b0:B) =>b = Some(b0))
    // and restore it to the previous state (issue19)
    FieldReferenceLinker._lastAccessedFieldReference = prev
    b.get
  }  
}
