package org.squeryl.pg

import org.squeryl._
import org.squeryl.adapters.PostgreSqlAdapter
import internals.{StatementWriter, FieldMapper}
import dsl.ast.{ViewExpressionNode, ExpressionNode}

class PgSchema(implicit fieldMapper: FieldMapper)
    extends Schema()(fieldMapper) {

  protected def srf[T]()(implicit man: Manifest[T]): ((ExpressionNode*) => View[T]) =
    srf(tableNameFromClass(man.erasure))(man)

  protected def srf[T](name: String)(implicit man: Manifest[T]): ((ExpressionNode*) => View[T]) =
    srf0(name, None, _: _*)

  protected def srf[T](name: String, prefix: String)(implicit man: Manifest[T]): ((ExpressionNode*) => View[T]) =
    srf0(name, Some(prefix), _: _*)

  private def srf0[T](name: String, prefix: Option[String], args: ExpressionNode*)(implicit man: Manifest[T]): View[T] = {
    val typeT = man.erasure.asInstanceOf[Class[T]]
    new SrfView[T](name, typeT, this, prefix, args)
  }
}

class SrfView[T](
    name: String,
    classOfT: Class[T],
    schema: Schema,
    prefix: Option[String],
    args: Iterable[ExpressionNode])
  extends View[T](
    name,
    classOfT,
    schema,
    prefix,
    None) {
  override def viewExpressionNode: ViewExpressionNode[T] = new SrfViewExpressionNode[T](this, args)
}

class SrfViewExpressionNode[T](view: View[T], args: Iterable[ExpressionNode]) extends ViewExpressionNode(view) {
  override def doWrite(sw: StatementWriter) = {
    sw.write(sw.quoteName(view.prefixedName))
    sw.write("(")
    sw.writeNodesWithSeparator(args, ",", false)
    sw.write(")")
  }
}
