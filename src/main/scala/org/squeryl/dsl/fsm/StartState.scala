package org.squeryl.dsl.fsm

import org.squeryl.dsl._
import ast.{TypedExpressionNode, LogicalBoolean, UpdateStatement, UpdateAssignment}
import boilerplate.{ComputeMeasuresSignaturesFromStartOrWhereState, ComputeMeasuresSignaturesFromGroupByState, GroupBySignatures, OrderBySignatures}


trait StartState
  extends GroupBySignatures
  with ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements =>

  def Select[R](yieldClosure: =>R): SelectState[R]
}

trait QueryElements
  extends WhereState
    with ComputeMeasuresSignaturesFromStartOrWhereState
    with StartState {

  private [squeryl] def whereClause:Option[()=>TypedExpressionNode[Scalar,LogicalBoolean]] = None
}

trait SelectState[R] extends QueryYield[R] with OrderBySignatures[R] {
  self: BaseQueryYield[R] =>
}

trait ComputeStateStartOrWhereState[M]
  extends QueryYield[Measures[M]]
    with OrderBySignatures[Measures[M]] {

  self: MeasuresQueryYield[M] =>
}

trait WhereState extends GroupBySignatures {
  self: QueryElements =>

  def Select[R](yieldClosure: =>R): SelectState[R] =
    new BaseQueryYield[R](this, yieldClosure _)

  def Set(updateAssignments: UpdateAssignment*) =
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

  def Having(b: =>TypedExpressionNode[Agregate,LogicalBoolean]): HavingState[K] = {
    _havingClause = Some(b _)
    this
  }
}
