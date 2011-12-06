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
import ast.{LogicalBoolean, UpdateStatement, UpdateAssignment}
import boilerplate.{ComputeMeasuresSignaturesFromStartOrWhereState, ComputeMeasuresSignaturesFromGroupByState, GroupBySignatures, OrderBySignatures}

abstract sealed class Conditioned
abstract sealed class Unconditioned

trait StartState
  extends GroupBySignatures
  with ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements[_] =>

  def select[R](yieldClosure: =>R): SelectState[R]
}

trait QueryElements[Cond]
  extends WhereState[Cond]
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

trait WhereState[Cond]
  extends GroupBySignatures 
  with ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements[_] =>

  def select[R](yieldClosure: =>R): SelectState[R] =
    new BaseQueryYield[R](this, yieldClosure _)

  def set(updateAssignments: UpdateAssignment*)(implicit cond: Cond =:= Conditioned) =
    new UpdateStatement(whereClause, updateAssignments)
  
  def setAll(updateAssignments: UpdateAssignment*)(implicit cond: Cond =:= Unconditioned) =
    new UpdateStatement(whereClause, updateAssignments)    
}

trait HavingState[G]
    extends ComputeMeasuresSignaturesFromGroupByState[G]  {
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

  def having(b: => LogicalBoolean) = {
    _havingClause = Some(b _)
    this
  }
}


