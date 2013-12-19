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

  def compute[T1,T2,T3,T4,T5,T6,T7,T8]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_]): ComputeStateFromGroupByState[G,Product8[T1,T2,T3,T4,T5,T6,T7,T8]] =
    new GroupWithMeasuresQueryYield[G,Product8[T1,T2,T3,T4,T5,T6,T7,T8]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_]): ComputeStateFromGroupByState[G,Product9[T1,T2,T3,T4,T5,T6,T7,T8,T9]] =
    new GroupWithMeasuresQueryYield[G,Product9[T1,T2,T3,T4,T5,T6,T7,T8,T9]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_]): ComputeStateFromGroupByState[G,Product10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]] =
    new GroupWithMeasuresQueryYield[G,Product10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_]): ComputeStateFromGroupByState[G,Product11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]] =
    new GroupWithMeasuresQueryYield[G,Product11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_]): ComputeStateFromGroupByState[G,Product12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]] =
    new GroupWithMeasuresQueryYield[G,Product12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_]): ComputeStateFromGroupByState[G,Product13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]] =
    new GroupWithMeasuresQueryYield[G,Product13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_]): ComputeStateFromGroupByState[G,Product14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]] =
    new GroupWithMeasuresQueryYield[G,Product14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_]): ComputeStateFromGroupByState[G,Product15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]] =
    new GroupWithMeasuresQueryYield[G,Product15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_]): ComputeStateFromGroupByState[G,Product16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]] =
    new GroupWithMeasuresQueryYield[G,Product16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_]): ComputeStateFromGroupByState[G,Product17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]] =
    new GroupWithMeasuresQueryYield[G,Product17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_]): ComputeStateFromGroupByState[G,Product18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]] =
    new GroupWithMeasuresQueryYield[G,Product18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_]): ComputeStateFromGroupByState[G,Product19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]] =
    new GroupWithMeasuresQueryYield[G,Product19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_]): ComputeStateFromGroupByState[G,Product20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]] =
    new GroupWithMeasuresQueryYield[G,Product20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_],
     e21: =>TypedExpression[T21,_]): ComputeStateFromGroupByState[G,Product21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]] =
    new GroupWithMeasuresQueryYield[G,Product21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]
    (e1: =>TypedExpression[T1,_], e2: =>TypedExpression[T2,_], e3: =>TypedExpression[T3,_], e4: =>TypedExpression[T4,_],
     e5: =>TypedExpression[T5,_], e6: =>TypedExpression[T6,_], e7: =>TypedExpression[T7,_], e8: =>TypedExpression[T8,_],
     e9: =>TypedExpression[T9,_], e10: =>TypedExpression[T10,_], e11: =>TypedExpression[T11,_], e12: =>TypedExpression[T12,_],
     e13: =>TypedExpression[T13,_], e14: =>TypedExpression[T14,_], e15: =>TypedExpression[T15,_], e16: =>TypedExpression[T16,_],
     e17: =>TypedExpression[T17,_], e18: =>TypedExpression[T18,_], e19: =>TypedExpression[T19,_], e20: =>TypedExpression[T20,_],
     e21: =>TypedExpression[T21,_], e22: =>TypedExpression[T22,_]): ComputeStateFromGroupByState[G,Product22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]] =
    new GroupWithMeasuresQueryYield[G,Product22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () =>List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22)
    )
}