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


  def compute[T1,T2,T3,T4,T5,T6,T7,T8]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_]): ComputeStateStartOrWhereState[Product8[T1,T2,T3,T4,T5,T6,T7,T8]] =
    new MeasuresQueryYield[Product8[T1,T2,T3,T4,T5,T6,T7,T8]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_]): ComputeStateStartOrWhereState[Product9[T1,T2,T3,T4,T5,T6,T7,T8,T9]] =
    new MeasuresQueryYield[Product9[T1,T2,T3,T4,T5,T6,T7,T8,T9]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_]): ComputeStateStartOrWhereState[Product10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]] =
    new MeasuresQueryYield[Product10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_]): ComputeStateStartOrWhereState[Product11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]] =
    new MeasuresQueryYield[Product11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_]): ComputeStateStartOrWhereState[Product12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]] =
    new MeasuresQueryYield[Product12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_]): ComputeStateStartOrWhereState[Product13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]] =
    new MeasuresQueryYield[Product13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_]): ComputeStateStartOrWhereState[Product14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]] =
    new MeasuresQueryYield[Product14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_]): ComputeStateStartOrWhereState[Product15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]] =
    new MeasuresQueryYield[Product15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_]): ComputeStateStartOrWhereState[Product16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]] =
    new MeasuresQueryYield[Product16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_]): ComputeStateStartOrWhereState[Product17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]] =
    new MeasuresQueryYield[Product17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_]): ComputeStateStartOrWhereState[Product18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]] =
    new MeasuresQueryYield[Product18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_]): ComputeStateStartOrWhereState[Product19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]] =
    new MeasuresQueryYield[Product19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_]): ComputeStateStartOrWhereState[Product20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]] =
    new MeasuresQueryYield[Product20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_],
     e21: =>TypedExpression[T21,_]): ComputeStateStartOrWhereState[Product21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]] =
    new MeasuresQueryYield[Product21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_],
     e21: =>TypedExpression[T21,_], e22: =>TypedExpression[T22,_]): ComputeStateStartOrWhereState[Product22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]] =
    new MeasuresQueryYield[Product22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22)
    )
}