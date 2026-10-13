package org.squeryl.dsl.ast

import org.squeryl.internals.*

class CastExpressionNode(expr: ExpressionNode, typ: String) extends ExpressionNode {
  override def doWrite(sw: StatementWriter): Unit = {
    sw.write("cast(")
    expr.write(sw)
    sw.write(s" as $typ)")
  }

  override def children: List[ExpressionNode] = List(expr)

  override def toString: String = s"'CastExpressionNode:$expr::$typ"
}
