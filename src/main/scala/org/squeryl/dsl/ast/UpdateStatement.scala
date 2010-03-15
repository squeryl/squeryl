package org.squeryl.dsl.ast

import org.squeryl.internals.StatementWriter

class UpdateStatement(val whereClause: Option[()=>LogicalBoolean], uas: Seq[UpdateAssignment])
   extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {}

  def columns =
    uas.map(ua => ua.left)

  def values =
    uas.map(ua => ua.right)
}
