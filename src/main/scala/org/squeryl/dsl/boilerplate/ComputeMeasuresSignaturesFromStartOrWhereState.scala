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

import org.squeryl.dsl.fsm.{MeasuresQueryYield, QueryElements, ComputeStateStartOrWhereState}
import org.squeryl.dsl.TypedExpression

trait ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements[_] =>

  def compute[T1](e1: =>TypedExpression[T1,_]): ComputeStateStartOrWhereState[T1] =
    new MeasuresQueryYield[T1](
      this,
      () =>List(e1)
    )

  def compute[T1,T2](e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_]): ComputeStateStartOrWhereState[Product2[T1,T2]] =
    new MeasuresQueryYield[Product2[T1,T2]](
      this,
      () =>List(e1, e2)
    )

  def compute[T1,T2,T3](e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_]): ComputeStateStartOrWhereState[Product3[T1,T2,T3]] =
    new MeasuresQueryYield[Product3[T1,T2,T3]](
      this,
      () =>List(e1, e2, e3)
    )

  def compute[T1,T2,T3,T4]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_]): ComputeStateStartOrWhereState[Product4[T1,T2,T3,T4]] =
    new MeasuresQueryYield[Product4[T1,T2,T3,T4]](
      this,
      () =>List(e1, e2, e3, e4)
    )

  def compute[T1,T2,T3,T4,T5]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_]): ComputeStateStartOrWhereState[Product5[T1,T2,T3,T4,T5]] =
    new MeasuresQueryYield[Product5[T1,T2,T3,T4,T5]](
      this,
      () =>List(e1, e2, e3, e4, e5)
    )

  def compute[T1,T2,T3,T4,T5,T6]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_]): ComputeStateStartOrWhereState[Product6[T1,T2,T3,T4,T5,T6]] =
    new MeasuresQueryYield[Product6[T1,T2,T3,T4,T5,T6]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_]): ComputeStateStartOrWhereState[Product7[T1,T2,T3,T4,T5,T6,T7]] =
    new MeasuresQueryYield[Product7[T1,T2,T3,T4,T5,T6,T7]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}
