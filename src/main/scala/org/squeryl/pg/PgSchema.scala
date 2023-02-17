package org.squeryl.pg

import org.squeryl._
import internals.{StatementWriter, FieldMapper}
import dsl.ast.{ViewExpressionNode, ExpressionNode}
import reflect.ClassTag

class PgSchema(implicit fieldMapper: FieldMapper)
    extends Schema()(fieldMapper) {

  protected def srf[T]()(implicit man: ClassTag[T]): (Seq[ExpressionNode] => View[T]) =
    srf(tableNameFromClass(man.runtimeClass))(man)

  protected def srf[T](name: String)(implicit man: ClassTag[T]): (Seq[ExpressionNode] => View[T]) =
    srf0(name, None, _: _*)

  protected def srf[T](name: String, prefix: String)(implicit man: ClassTag[T]): (Seq[ExpressionNode] => View[T]) =
    srf0(name, Some(prefix), _: _*)

  private def srf0[T](name: String, prefix: Option[String], args: ExpressionNode*)(implicit man: ClassTag[T]): View[T] = {
    val typeT = man.runtimeClass.asInstanceOf[Class[T]]
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
    None,
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
