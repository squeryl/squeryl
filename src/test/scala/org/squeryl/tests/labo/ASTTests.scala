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
package org.squeryl.tests

import org.squeryl.{Table}

object ASTTests {
  import org.squeryl.PrimitiveTypeMode._

  class T(var x: Int) {
    def this() = this(0)
  }
  class U(var y: Int) {
    def this() = this(0)
  }
  class V(var z: Int) {
    def this() = this(0)
  }

  val ts = new Table[T]("T")
  val us = new Table[U]("U")
  val vs = new Table[V]("V")

  val q0 =
    from(us,ts)((u,t) =>
      where(t.x === u.y)
      select((t,u))
    )
  
  val q1 =
    from(vs, q0)((v,q) =>
      where(v.z === q._1.x)
      select((v,q))
    )



  def test1 = {

//    for(n <- q1.ast.filterDescendantsOfType[ReferenceExpressionNode])
//      println(n)

    println(q1.dumpAst)
    println(q1)
  }

  //class LetExpression[U]
  //def Let[U](t: Table[U])(f: )

//  case class Sub[U](q: Queryable[U]) extends Queryable[U] {
//    def name = q.name
//    private[squeryl] def give(rsm: ResultSetMapper) = q.give(rsm)
//    var whereClause: ()=>ScalarBoolean = null
//  }
//
//  def WhereSub[T](t: T, b: =>ScalarBoolean) = {
//      //whereClause = b _
//      ~:
//  }
//
//  def let = {
//
//    val q1 =
//      From(ts,Sub(us))((t,u) =>
//        WhereSub(u, u.y === t.x)
//        Where(t.x === u.y)
//        Select((t,u))
//      )
//  }


//  case class Subz[U](q: Queryable[U]) extends Queryable[Queryable[U]] {
//    def name = q.name
//    private[squeryl] def give(rsm: ResultSetMapper) = error("!!!")
//    var whereClause: ()=>ScalarBoolean = null
//    def Where(b: =>ScalarBoolean) = {
//      whereClause = b _
//        ~:
//    }
//
//  }
//
//  def let2 = {
//
//    val q1 =
//      From(ts,Subz(us))((t,subU) =>
//        subU.Where(u => u.y === t.x)
//        Where(t.x === u.cols.y)
//        Select((t,u))
//      )
//  }

//  val sq3 =
//    From(ts)(t=>
//     ~:Where(
//         t.x in From(us)(u =>
//                  ~:Where(u.y === t.x) Select(u.y)
//                )
//       )
//      Select(t)
//    )
}
