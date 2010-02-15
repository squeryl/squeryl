package org.squeryl.dsl.ast


class OuterJoinExpression(val queryableExpressionNode: QueryableExpressionNode, val leftRightOrFull: String, val matchExpression: ExpressionNode) {

  def inhibited = queryableExpressionNode.inhibited 
}