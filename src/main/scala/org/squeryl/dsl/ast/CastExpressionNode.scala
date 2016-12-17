package org.squeryl.dsl.ast

import org.squeryl.internals._

class CastExpressionNode(expr: ExpressionNode,
                         typ: String) extends ExpressionNode {
  override def doWrite(sw: StatementWriter) = {
    sw.write("cast(")
    expr.write(sw)
    sw.write(" as " + sw.databaseAdapter.quoteIdentifier(typ) + ")")
  }

  override def children = List(expr)

  override def toString = 'CastExpressionNode + ":" + expr.toString + "::" + typ
}
