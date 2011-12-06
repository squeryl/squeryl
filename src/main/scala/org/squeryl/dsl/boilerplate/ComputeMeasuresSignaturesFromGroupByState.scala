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

import org.squeryl.dsl.fsm._
import org.squeryl.dsl.TypedExpression

trait ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>

  def compute[T1](e1: =>TypedExpression[T1,_]): ComputeStateFromGroupByState[G,T1] =
    new GroupWithMeasuresQueryYield[G,T1](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1)
    )

  def compute[T1,T2](e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_]): ComputeStateFromGroupByState[G,Product2[T1,T2]] =
    new GroupWithMeasuresQueryYield[G,Product2[T1,T2]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2)
    )

  def compute[T1,T2,T3](e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_]): ComputeStateFromGroupByState[G,Product3[T1,T2,T3]] =
    new GroupWithMeasuresQueryYield[G,Product3[T1,T2,T3]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3)
    )

  def compute[T1,T2,T3,T4](e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_]): ComputeStateFromGroupByState[G,Product4[T1,T2,T3,T4]] =
    new GroupWithMeasuresQueryYield[G,Product4[T1,T2,T3,T4]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4)
    )

  def compute[T1,T2,T3,T4,T5]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_]): ComputeStateFromGroupByState[G,Product5[T1,T2,T3,T4,T5]] =
    new GroupWithMeasuresQueryYield[G,Product5[T1,T2,T3,T4,T5]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5)
    )

  def compute[T1,T2,T3,T4,T5,T6]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_]): ComputeStateFromGroupByState[G,Product6[T1,T2,T3,T4,T5,T6]] =
    new GroupWithMeasuresQueryYield[G,Product6[T1,T2,T3,T4,T5,T6]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_]): ComputeStateFromGroupByState[G,Product7[T1,T2,T3,T4,T5,T6,T7]] =
    new GroupWithMeasuresQueryYield[G,Product7[T1,T2,T3,T4,T5,T6,T7]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}
