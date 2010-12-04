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
package org.squeryl.dsl.fsm

import org.squeryl.dsl._
import ast.{TypedExpressionNode, LogicalBoolean, UpdateStatement, UpdateAssignment}
import boilerplate.{ComputeMeasuresSignaturesFromStartOrWhereState, ComputeMeasuresSignaturesFromGroupByState, GroupBySignatures, OrderBySignatures}


trait StartState
  extends GroupBySignatures
  with ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements =>

  def select[R](yieldClosure: =>R): SelectState[R]
}

trait QueryElements
  extends WhereState
    with ComputeMeasuresSignaturesFromStartOrWhereState
    with StartState {

  private [squeryl] def whereClause:Option[()=>LogicalBoolean] = None
}

trait SelectState[R] extends QueryYield[R] with OrderBySignatures[R] {
  self: BaseQueryYield[R] =>
}

trait ComputeStateStartOrWhereState[M]
  extends QueryYield[Measures[M]]
    with OrderBySignatures[Measures[M]] {

  self: MeasuresQueryYield[M] =>
}

trait WhereState
  extends GroupBySignatures 
  with ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements =>

  def select[R](yieldClosure: =>R): SelectState[R] =
    new BaseQueryYield[R](this, yieldClosure _)

  def set(updateAssignments: UpdateAssignment*) =
    new UpdateStatement(whereClause, updateAssignments )
}

trait HavingState[G]
    extends ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>
}

trait ComputeStateFromGroupByState[K,M]
  extends QueryYield[GroupWithMeasures[K,M]] with OrderBySignatures[GroupWithMeasures[K,M]] {
  self: GroupWithMeasuresQueryYield[K,M] =>
}


trait GroupByState[K]
  extends QueryYield[Group[K]]
  with ComputeMeasuresSignaturesFromGroupByState[K]
  with OrderBySignatures[Group[K]] {
  self: GroupQueryYield[K] =>

  def having(b: =>TypedExpressionNode[LogicalBoolean]): HavingState[K] = {
    _havingClause = Some(b _)
    this
  }
}


