package org.squeryl.dsl.ast

trait QueryExpressionElements extends ExpressionNode {

  var inhibitAliasOnSelectElementReference = false

  def isChild(q: QueryableExpressionNode): Boolean

  def alias: String

  def selectDistinct: Boolean

  def isForUpdate: Boolean

  def views: Iterable[QueryableExpressionNode]

  def joinTableExpressions: Iterable[QueryableExpressionNode]

  def subQueries: Iterable[QueryableExpressionNode]
  //TODO: rename fromList ?
  def tableExpressions: Iterable[QueryableExpressionNode]

  def selectList: Iterable[SelectElement]

  def whereClause: Option[ExpressionNode]

  def havingClause: Option[ExpressionNode]

  def groupByClause: Iterable[ExpressionNode]

  def orderByClause: Iterable[ExpressionNode]
}
