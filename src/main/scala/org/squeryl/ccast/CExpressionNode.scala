package org.squeryl.ccast

trait CExpressionNode

trait CTableOrQuery extends CExpressionNode

case class CTable(prefix: Option[String], name: String) extends CTableOrQuery

case class CQueryExpressionNode(
  select: Seq[CSelectElement],
  subs: Seq[CTableOrQuery],
  where: Option[CExpressionNode]
) extends CTableOrQuery

case class CBinaryOperatorNode(left: CExpressionNode, right: CExpressionNode, op: String) extends CExpressionNode

case class CConstantTypedExpression[A1](val value: A1, nativeJdbcValue: AnyRef) extends CExpressionNode

trait CSelectElement extends CExpressionNode

case class ResultSetCol(idx: Int, nativeJDBClass: Class[_])

case class CSelectElementReference(selectElement: CSelectElement, resultSetCol: ResultSetCol) extends CSelectElement

case class CFieldSelectElement(origin: CTable, colName: String, resultSetCol: ResultSetCol) extends CSelectElement

case class CExportedSelectElement(selectElement: CSelectElement) extends CSelectElement
