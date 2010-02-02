package org.squeryl.dsl.ast

import org.squeryl.dsl.Scalar
import org.squeryl.internals.StatementWriter

class UpdateStatement(private val _cols: Iterable[ExpressionNode], private val _values: ExpressionNode*)
   extends ExpressionNode {

  private var _whereClause: Option[()=>TypedExpressionNode[Scalar,LogicalBoolean]] = None

  def doWrite(sw: StatementWriter) = {}
  
  def Where(clause: =>TypedExpressionNode[Scalar,LogicalBoolean]) = {
    _whereClause = Some(clause _)
    this
  }
  
  def columns = 
    _cols.map(c=>c.asInstanceOf[SelectElementReference].selectElement.asInstanceOf[FieldSelectElement].fieldMataData)

  def values = _values.toList

  def whereClause = _whereClause
}
