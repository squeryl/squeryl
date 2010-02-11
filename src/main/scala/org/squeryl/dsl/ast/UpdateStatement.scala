package org.squeryl.dsl.ast

import org.squeryl.dsl.Scalar
import org.squeryl.internals.StatementWriter

class UpdateStatement(val whereClause: Option[()=>TypedExpressionNode[LogicalBoolean]], uas: Seq[UpdateAssignment])
   extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {}

  def columns =
    uas.map(ua => ua.left.asInstanceOf[SelectElementReference].selectElement.asInstanceOf[FieldSelectElement].fieldMataData)

  def values =
    uas.map(ua => ua.right)
}
