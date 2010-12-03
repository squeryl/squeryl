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

import org.squeryl.dsl.fsm.{GroupQueryYield, QueryElements, GroupByState}
import org.squeryl.dsl.ast.{TypedExpressionNode}

trait GroupBySignatures {
  self: QueryElements =>

  def groupBy[T1](e1: =>TypedExpressionNode[T1]): GroupByState[T1] =
    new GroupQueryYield[T1](this,
      ()=>List(e1)
    )

  def groupBy[T1,T2](e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2]): GroupByState[(T1,T2)] =
    new GroupQueryYield[(T1,T2)](this,
      ()=>List(e1, e2)
    )

  def groupBy[T1,T2,T3](e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3]): GroupByState[(T1,T2,T3)] =
    new GroupQueryYield[(T1,T2,T3)](this,
      ()=>List(e1, e2, e3)
    )

  def groupBy[T1,T2,T3,T4](e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3], e4: =>TypedExpressionNode[T4]): GroupByState[(T1,T2,T3,T4)] =
    new GroupQueryYield[(T1,T2,T3,T4)](this,
      ()=>List(e1, e2, e3, e4)
    )

  def groupBy[T1,T2,T3,T4,T5](e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3], e4: =>TypedExpressionNode[T4], e5: =>TypedExpressionNode[T5]): GroupByState[(T1,T2,T3,T4,T5)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5)](this,
      ()=>List(e1, e2, e3, e4, e5)
    )

  def groupBy[T1,T2,T3,T4,T5,T6]
    (e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3], e4: =>TypedExpressionNode[T4],
     e5: =>TypedExpressionNode[T5], e6: =>TypedExpressionNode[T6]):
     GroupByState[(T1,T2,T3,T4,T5,T6)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6)](this,
      ()=>List(e1, e2, e3, e4, e5, e6)
    )

  def groupBy[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3], e4: =>TypedExpressionNode[T4],
     e5: =>TypedExpressionNode[T5], e6: =>TypedExpressionNode[T6], e7: =>TypedExpressionNode[T7]):
     GroupByState[(T1,T2,T3,T4,T5,T6,T7)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6,T7)](this,
      ()=>List(e1, e2, e3, e4, e5, e6, e7)
    )

  def groupBy[T1,T2,T3,T4,T5,T6,T7,T8]
    (e1: =>TypedExpressionNode[T1], e2: =>TypedExpressionNode[T2], e3: =>TypedExpressionNode[T3], e4: =>TypedExpressionNode[T4],
     e5: =>TypedExpressionNode[T5], e6: =>TypedExpressionNode[T6], e7: =>TypedExpressionNode[T7], e8: =>TypedExpressionNode[T8]):
     GroupByState[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6,T7,T8)](this,
      ()=>List(e1, e2, e3, e4, e5, e6, e7, e8)
    )
}
