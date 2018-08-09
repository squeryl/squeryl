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
package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.{QueryYield}
import org.squeryl.dsl.internal.{JoinedQueryable, InnerJoinedQueryable, OuterJoinedQueryable}
import org.squeryl.{Queryable, Query}

trait JoinSignatures {
  self: FromSignatures =>
  
  class JoinPrecursor[A](q: Queryable[A]) {
    def leftOuter = new OuterJoinedQueryable[A](q, "left")
    def rightOuter = new OuterJoinedQueryable[A](q, "right")
    def fullOuter = new OuterJoinedQueryable[A](q, "full")
  }

  implicit def queryable2JoinPrecursor[A](q: Queryable[A]) = new JoinPrecursor[A](q)

  implicit def queryable2RightInnerJoinedQueryable[A](q: Queryable[A]) = new InnerJoinedQueryable[A](q, "")

  def join[A,B1,C](q: Queryable[A], q1: JoinedQueryable[B1])(f: Function2[A,B1,JoinQueryYield1[C]]): Query[C] =
    from(q,q1)(
     (a:A,b1:B1) => f(a,b1).queryYield
    )

  def join[A,B1,B2,C](q: Queryable[A], q1: JoinedQueryable[B1], q2: JoinedQueryable[B2])(f: Function3[A,B1,B2,JoinQueryYield2[C]]): Query[C] =
    from(q,q1,q2)(
      (a:A,b1:B1,b2:B2) => f(a,b1,b2).queryYield
    )

  def join[A,B1,B2,B3,C](q: Queryable[A], q1: JoinedQueryable[B1], q2: JoinedQueryable[B2], q3: JoinedQueryable[B3])(f: Function4[A,B1,B2,B3,JoinQueryYield3[C]]): Query[C] =
    from(q,q1,q2,q3)(
      (a:A,b1:B1,b2:B2,b3:B3) => f(a,b1,b2,b3).queryYield
    )

  def join[A,B1,B2,B3,B4,C](q: Queryable[A], q1: JoinedQueryable[B1], q2: JoinedQueryable[B2], q3: JoinedQueryable[B3], q4: JoinedQueryable[B4])(
          f: Function5[A,B1,B2,B3,B4,JoinQueryYield4[C]]): Query[C] =
    from(q,q1,q2,q3,q4)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4) => f(a,b1,b2,b3,b4).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,C](          
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5])(
          f: Function6[A,B1,B2,B3,B4,B5,JoinQueryYield5[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5) =>
        f(a,b1,b2,b3,b4,b5).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6])(
          f: Function7[A,B1,B2,B3,B4,B5,B6,JoinQueryYield6[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6) =>
        f(a,b1,b2,b3,b4,b5,b6).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7])(
          f: Function8[A,B1,B2,B3,B4,B5,B6,B7,JoinQueryYield7[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7) =>
        f(a,b1,b2,b3,b4,b5,b6,b7).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8])(
          f: Function9[A,B1,B2,B3,B4,B5,B6,B7,B8,JoinQueryYield8[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8],
          q9: JoinedQueryable[B9])(
          f: Function10[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,JoinQueryYield9[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8,q9)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8,b9:B9) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8,b9).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8],
          q9: JoinedQueryable[B9],
          q10: JoinedQueryable[B10])(
          f: Function11[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,JoinQueryYield10[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8,b9:B9,b10:B10) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8],
          q9: JoinedQueryable[B9],
          q10: JoinedQueryable[B10],
          q11: JoinedQueryable[B11])(
          f: Function12[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,JoinQueryYield11[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8,b9:B9,b10:B10,b11:B11) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8],
          q9: JoinedQueryable[B9],
          q10: JoinedQueryable[B10],
          q11: JoinedQueryable[B11],
          q12: JoinedQueryable[B12])(
          f: Function13[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,JoinQueryYield12[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8,b9:B9,b10:B10,b11:B11,b12:B12) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12).queryYield
    )

  def join[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,C](
          q: Queryable[A],
          q1: JoinedQueryable[B1],
          q2: JoinedQueryable[B2],
          q3: JoinedQueryable[B3],
          q4: JoinedQueryable[B4],
          q5: JoinedQueryable[B5],
          q6: JoinedQueryable[B6],
          q7: JoinedQueryable[B7],
          q8: JoinedQueryable[B8],
          q9: JoinedQueryable[B9],
          q10: JoinedQueryable[B10],
          q11: JoinedQueryable[B11],
          q12: JoinedQueryable[B12],
          q13: JoinedQueryable[B13])(
          f: Function14[A,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,JoinQueryYield13[C]]): Query[C] =
    from(q,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13)(
      (a:A,b1:B1,b2:B2,b3:B3,b4:B4,b5:B5,b6:B6,b7:B7,b8:B8,b9:B9,b10:B10,b11:B11,b12:B12,b13:B13) =>
        f(a,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13).queryYield
    )
}


class JoinQueryYield1[R](val queryYield: QueryYield[R])
class JoinQueryYield2[R](val queryYield: QueryYield[R])
class JoinQueryYield3[R](val queryYield: QueryYield[R])
class JoinQueryYield4[R](val queryYield: QueryYield[R])
class JoinQueryYield5[R](val queryYield: QueryYield[R])
class JoinQueryYield6[R](val queryYield: QueryYield[R])
class JoinQueryYield7[R](val queryYield: QueryYield[R])
class JoinQueryYield8[R](val queryYield: QueryYield[R])
class JoinQueryYield9[R](val queryYield: QueryYield[R])
class JoinQueryYield10[R](val queryYield: QueryYield[R])
class JoinQueryYield11[R](val queryYield: QueryYield[R])
class JoinQueryYield12[R](val queryYield: QueryYield[R])
class JoinQueryYield13[R](val queryYield: QueryYield[R])
