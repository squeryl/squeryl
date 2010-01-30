package org.squeryl.dsl

import ast.{ExpressionNode, QueryExpressionNode, SelectElement}
import org.squeryl.internals.{ResultSetMapper}
import java.sql.ResultSet

trait QueryYield[R] {

  def invokeYield(resultSetMapper: ResultSetMapper, rs: ResultSet): R

  def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper):
    (Iterable[SelectElement],AnyRef)

  def queryElements:
    (Option[ExpressionNode],
     Option[ExpressionNode],
     Iterable[ExpressionNode],
     Iterable[ExpressionNode])  
}
