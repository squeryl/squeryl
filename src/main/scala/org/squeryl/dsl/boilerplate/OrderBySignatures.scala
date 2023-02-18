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

import org.squeryl.dsl.QueryYield
import org.squeryl.dsl.fsm.BaseQueryYield
import org.squeryl.dsl.ast.ExpressionNode

trait OrderBySignatures[R] {
  self: BaseQueryYield[R] =>

  type O = ExpressionNode

  def orderBy(args: List[O]): QueryYield[R] = {
    _orderByExpressions = () => args.map(() => _)
    this
  }

  def orderBy(e1: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1)
    this
  }

  def orderBy(e1: => O, e2: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2)
    this
  }

  def orderBy(e1: => O, e2: => O, e3: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2, () => e3)
    this
  }

  def orderBy(e1: => O, e2: => O, e3: => O, e4: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2, () => e3, () => e4)
    this
  }

  def orderBy(e1: => O, e2: => O, e3: => O, e4: => O, e5: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2, () => e3, () => e4, () => e5)
    this
  }

  def orderBy(e1: => O, e2: => O, e3: => O, e4: => O, e5: => O, e6: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2, () => e3, () => e4, () => e5, () => e6)
    this
  }

  def orderBy(e1: => O, e2: => O, e3: => O, e4: => O, e5: => O, e6: => O, e7: => O): QueryYield[R] = {
    _orderByExpressions = () => List(() => e1, () => e2, () => e3, () => e4, () => e5, () => e6, () => e7)
    this
  }
}
