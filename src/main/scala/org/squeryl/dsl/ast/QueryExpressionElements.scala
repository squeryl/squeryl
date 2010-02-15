package org.squeryl.dsl.ast

trait QueryExpressionElements extends ExpressionNode {

  var inhibitAliasOnSelectElementReference = false

  def isChild(q: QueryableExpressionNode): Boolean

  def alias: String

  def selectDistinct: Boolean

  def isForUpdate: Boolean

  def views: Iterable[QueryableExpressionNode]

  def outerJoinExpressions: Iterable[OuterJoinExpression]

  def subQueries: Iterable[QueryableExpressionNode]

  def tableExpressions: Iterable[QueryableExpressionNode]

  def selectList: Iterable[SelectElement]

  def whereClause: Option[ExpressionNode]

  def havingClause: Option[ExpressionNode]

  def groupByClause: Iterable[ExpressionNode]

  def orderByClause: Iterable[ExpressionNode]
}
