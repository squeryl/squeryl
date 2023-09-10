/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.dsl.ast

import scala.collection.mutable.ArrayBuffer

import org.squeryl.internals._
import org.squeryl.dsl._
import org.squeryl.Session
import reflect.ClassTag

trait ExpressionNode {

  var parent: Option[ExpressionNode] = None

  def id = Integer.toHexString(System.identityHashCode(this))

  def inhibited = _inhibitedByWhen

  def inhibitedFlagForAstDump =
    if (inhibited) "!" else ""

  def write(sw: StatementWriter) =
    if (!inhibited)
      doWrite(sw)

  def doWrite(sw: StatementWriter): Unit

  def writeToString: String = {
    val sw = new StatementWriter(Session.currentSession.databaseAdapter)
    write(sw)
    sw.statement
  }

  def children: List[ExpressionNode] = List.empty

  override def toString = this.getClass.getName

  private def _visitDescendants(
    n: ExpressionNode,
    parent: Option[ExpressionNode],
    depth: Int,
    visitor: (ExpressionNode, Option[ExpressionNode], Int) => Unit
  ): Unit = {
    visitor(n, parent, depth)
    n.children.foreach(child => _visitDescendants(child, Some(n), depth + 1, visitor))
  }

  private def _filterDescendants(
    n: ExpressionNode,
    ab: ArrayBuffer[ExpressionNode],
    predicate: (ExpressionNode) => Boolean
  ): Iterable[ExpressionNode] = {
    if (predicate(n))
      ab.append(n)
    n.children.foreach(child => _filterDescendants(child, ab, predicate))
    ab
  }

  def filterDescendants(predicate: (ExpressionNode) => Boolean) =
    _filterDescendants(this, new ArrayBuffer[ExpressionNode], predicate)

  def filterDescendantsOfType[T](implicit ClassTag: ClassTag[T]) =
    _filterDescendants(
      this,
      new ArrayBuffer[ExpressionNode],
      (n: ExpressionNode) => ClassTag.runtimeClass.isAssignableFrom(n.getClass)
    ).asInstanceOf[Iterable[T]]

  /**
   * visitor's args are :
   *  -the visited node,
   *  -it's parent
   *  -it's depth
   */
  def visitDescendants(visitor: (ExpressionNode, Option[ExpressionNode], Int) => Unit) =
    _visitDescendants(this, None, 0, visitor)

  protected var _inhibitedByWhen = false

  def inhibitWhen(inhibited: Boolean): this.type = {
    _inhibitedByWhen = inhibited
    this
  }

  def ? : this.type = {
    if (!this.isInstanceOf[ConstantTypedExpression[_, _]])
      throw new UnsupportedOperationException(
        "the '?' operator (shorthand for 'p.inhibitWhen(p == None))' can only be used on a constant query argument"
      )

    val c = this.asInstanceOf[ConstantTypedExpression[_, _]]

    inhibitWhen(c.value == None)
  }

  def cast[A, T](typ: String)(implicit tef: TypedExpressionFactory[A, T]): TypedExpression[A, T] =
    new CastExpressionNode(this, typ) with TypedExpression[A, T] {
      override def mapper = tef.createOutMapper
    }
}

class ListExpressionNode(override val children: List[ExpressionNode]) extends ExpressionNode {
  override def doWrite(sw: StatementWriter) = {
    sw.writeNodesWithSeparator(children, ", ", false)
  }
}

class RowValueConstructorNode(override val children: List[ExpressionNode]) extends ExpressionNode {
  override def doWrite(sw: StatementWriter) = {
    // sw.write("ROW")
    sw.write("(")
    sw.writeNodesWithSeparator(children, ", ", false)
    sw.write(")")
  }
}

class EqualityExpression(override val left: TypedExpression[_, _], override val right: TypedExpression[_, _])
    extends BinaryOperatorNodeLogicalBoolean(left, right, "=") {

  override def doWrite(sw: StatementWriter) =
    right match {
      case c: ConstantTypedExpression[_, _] =>
        if (c.value == None) {
          left.write(sw)
          sw.write(" is null")
        } else super.doWrite(sw)
      case _ => super.doWrite(sw)
    }

}

class InclusionOperator(left: ExpressionNode, right: RightHandSideOfIn[_])
    extends BinaryOperatorNodeLogicalBoolean(left, right, "in", true) {

  override def doWrite(sw: StatementWriter) =
    if (right.isConstantEmptyList)
      sw.write("(1 = 0)")
    else
      super.doWrite(sw)
}

class ExclusionOperator(left: ExpressionNode, right: RightHandSideOfIn[_])
    extends BinaryOperatorNodeLogicalBoolean(left, right, "not in", true)

class BinaryOperatorNodeLogicalBoolean(
  left: ExpressionNode,
  right: ExpressionNode,
  op: String,
  rightArgInParent: Boolean = false
) extends BinaryOperatorNode(left, right, op)
    with LogicalBoolean {

  override def inhibited = _inhibitedByWhen || {
    left match {
      case _: LogicalBoolean =>
        left.inhibited && right.inhibited
      case _ =>
        left.inhibited || right.inhibited
    }
  }

  override def doWrite(sw: StatementWriter) = {
    // since we are executing this method, we have at least one non inhibited children
    val nonInh = children.filter(c => !c.inhibited).iterator

    sw.write("(")
    nonInh.next().write(sw)
    sw.write(" ")
    if (nonInh.hasNext) {
      sw.write(operatorToken)
      if (newLineAfterOperator)
        sw.nextLine
      sw.write(" ")

      if (rightArgInParent)
        sw.write("(")

      nonInh.next().write(sw)

      if (rightArgInParent)
        sw.write(")")
    }
    sw.write(")")
  }
}

class ExistsExpression(val ast: ExpressionNode, val opType: String)
    extends PrefixOperatorNode(ast, opType, false)
    with LogicalBoolean

class BetweenExpression(first: ExpressionNode, second: ExpressionNode, third: ExpressionNode)
    extends TernaryOperatorNode(first, second, third, "between")
    with LogicalBoolean {

  override def doWrite(sw: StatementWriter) = {
    first.write(sw)
    sw.write(" between ")
    second.write(sw)
    sw.write(" and ")
    third.write(sw)
  }
}

class TernaryOperatorNode(val first: ExpressionNode, val second: ExpressionNode, val third: ExpressionNode, op: String)
    extends FunctionNode(op, Seq(first, second, third))
    with LogicalBoolean {

  override def inhibited =
    _inhibitedByWhen || first.inhibited || second.inhibited || third.inhibited
}

trait LogicalBoolean extends ExpressionNode {

  def and(b: LogicalBoolean): LogicalBoolean =
    new BinaryOperatorNodeLogicalBoolean(this, b, "and")

  def or(b: LogicalBoolean): LogicalBoolean =
    new BinaryOperatorNodeLogicalBoolean(this, b, "or")

  def and(b: Option[LogicalBoolean]): LogicalBoolean =
    b.map(this and _).getOrElse(this)

  def or(b: Option[LogicalBoolean]): LogicalBoolean =
    b.map(this or _).getOrElse(this)

}

object TrueLogicalBoolean extends LogicalBoolean {

  override def and(b: LogicalBoolean): LogicalBoolean = b

  override def or(b: LogicalBoolean): LogicalBoolean = this

  override def doWrite(sw: StatementWriter) = {
    sw.write("(1=1)")
  }

}

object FalseLogicalBoolean extends LogicalBoolean {

  override def and(b: LogicalBoolean): LogicalBoolean = this

  override def or(b: LogicalBoolean): LogicalBoolean = b

  override def doWrite(sw: StatementWriter) = {
    sw.write("(1=0)")
  }

}

object LogicalBoolean {

  def and(conditions: collection.Seq[LogicalBoolean]): LogicalBoolean =
    conditions.foldLeft[LogicalBoolean](TrueLogicalBoolean)(_ and _)

  def or(conditions: collection.Seq[LogicalBoolean]): LogicalBoolean =
    conditions.foldLeft[LogicalBoolean](FalseLogicalBoolean)(_ or _)

}

class UpdateAssignment(val left: FieldMetaData, val right: ExpressionNode)

trait BaseColumnAttributeAssignment {

  def clearColumnAttributes: Unit

  def isIdFieldOfKeyedEntity: Boolean

  def isIdFieldOfKeyedEntityWithoutUniquenessConstraint =
    isIdFieldOfKeyedEntity && !(columnAttributes.exists(_.isInstanceOf[PrimaryKey]) || columnAttributes.exists(
      _.isInstanceOf[Unique]
    ))

  def columnAttributes: collection.Seq[ColumnAttribute]

  def hasAttribute[A <: ColumnAttribute](implicit m: ClassTag[A]) =
    findAttribute[A](m).isDefined

  def findAttribute[A <: ColumnAttribute](implicit m: ClassTag[A]) =
    columnAttributes.find(ca => m.runtimeClass.isAssignableFrom(ca.getClass))
}

class ColumnGroupAttributeAssignment(
  cols: collection.Seq[FieldMetaData],
  columnAttributes_ : collection.Seq[ColumnAttribute]
) extends BaseColumnAttributeAssignment {

  private[this] val _columnAttributes = new ArrayBuffer[ColumnAttribute]

  _columnAttributes ++= columnAttributes_

  def columnAttributes: collection.Seq[ColumnAttribute] = _columnAttributes

  def addAttribute(a: ColumnAttribute) =
    _columnAttributes.append(a)

  def clearColumnAttributes = columns.foreach(_._clearColumnAttributes)

  def columns: collection.Seq[FieldMetaData] = cols

  def isIdFieldOfKeyedEntity = false

  def name: Option[String] = None
}

class CompositeKeyAttributeAssignment(val group: CompositeKey, _columnAttributes: collection.Seq[ColumnAttribute])
    extends ColumnGroupAttributeAssignment(group._fields, _columnAttributes) {

  override def isIdFieldOfKeyedEntity = {
    val fmdHead = group._fields.head
    fmdHead.parentMetaData.viewOrTable.ked.exists(k => Some(k.idPropertyName) == group._propertyName)
  }

  assert(group._propertyName.isDefined)

  override def name: Option[String] = group._propertyName
}

class ColumnAttributeAssignment(val left: FieldMetaData, val columnAttributes: collection.Seq[ColumnAttribute])
    extends BaseColumnAttributeAssignment {

  def clearColumnAttributes = left._clearColumnAttributes

  def isIdFieldOfKeyedEntity = left.isIdFieldOfKeyedEntity
}

class DefaultValueAssignment(val left: FieldMetaData, val value: TypedExpression[_, _])
    extends BaseColumnAttributeAssignment {

  def isIdFieldOfKeyedEntity = left.isIdFieldOfKeyedEntity

  def clearColumnAttributes = left._clearColumnAttributes

  def columnAttributes: Seq[ColumnAttribute] = Nil
}

class TokenExpressionNode(val token: String) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = sw.write(token)
}

private[squeryl] class InputOnlyConstantExpressionNode(v: Any)
    extends ConstantTypedExpression[Any, Any](v, v.asInstanceOf[AnyRef], None)

class ConstantTypedExpression[A1, T1](
  val value: A1,
  val nativeJdbcValue: AnyRef,
  i: Option[TypedExpressionFactory[A1, _]]
) extends TypedExpression[A1, T1] {

  private def needsQuote = value.isInstanceOf[String]

  override def mapper: OutMapper[A1] = i.get.createOutMapper

  override def sample =
    if (value != null) value
    else i.get.sample

  def jdbcClass =
    i.map(_.jdbcSample).getOrElse(nativeJdbcValue).getClass

    if (nativeJdbcValue != null) nativeJdbcValue.getClass
    else mapper.jdbcClass

  def doWrite(sw: StatementWriter) = {
    if (sw.isForDisplay) {
      sw.write(displayAsString)
    } else {
      sw.write("?")
      sw.addParam(ConstantStatementParam(this))
    }
  }

  def displayAsString =
    if (value == null)
      "null"
    else if (needsQuote)
      "'" + value.toString + "'"
    else
      value.toString

  override def toString = "'ConstantTypedExpression:" + value
}

class ConstantExpressionNodeList[T](val value: Iterable[T], mapper: OutMapper[_]) extends ExpressionNode {

  def isEmpty =
    value == Nil

  def doWrite(sw: StatementWriter) =
    if (sw.isForDisplay)
      sw.write(ConstantExpressionNodeList.this.value.map(e => "'" + e + "'").mkString(","))
    else {
      sw.write(ConstantExpressionNodeList.this.value.toSeq.map(z => "?").mkString(","))
      ConstantExpressionNodeList.this.value.foreach(z =>
        sw.addParam(ConstantExpressionNodeListParam(z.asInstanceOf[AnyRef], ConstantExpressionNodeList.this))
      )
    }
}

class FunctionNode(val name: String, val args: collection.Seq[ExpressionNode]) extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {

    sw.write(name)
    sw.write("(")
    sw.writeNodesWithSeparator(args, ",", false)
    sw.write(")")
  }

  override def children = args.toList
}

class PostfixOperatorNode(val token: String, val arg: ExpressionNode) extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {
    arg.write(sw)
    sw.write(" ")
    sw.write(token)
  }

  override def children = List(arg)
}

class TypeConversion(e: ExpressionNode) extends ExpressionNode {

  override def inhibited = e.inhibited

  override def doWrite(sw: StatementWriter) = e.doWrite((sw))

  override def children = e.children
}

class BinaryOperatorNode(
  val left: ExpressionNode,
  val right: ExpressionNode,
  val operatorToken: String,
  val newLineAfterOperator: Boolean = false
) extends ExpressionNode {

  override def children = List(left, right)

  override def inhibited =
    _inhibitedByWhen || left.inhibited || right.inhibited

  override def toString =
    "'BinaryOperatorNode:" + operatorToken + inhibitedFlagForAstDump

  def doWrite(sw: StatementWriter) = {
    sw.write("(")
    left.write(sw)
    sw.write(" ")
    sw.write(operatorToken)
    if (newLineAfterOperator)
      sw.nextLine
    sw.write(" ")
    right.write(sw)
    sw.write(")")
  }
}

class PrefixOperatorNode(
  val child: ExpressionNode,
  val operatorToken: String,
  val newLineAfterOperator: Boolean = false
) extends ExpressionNode {

  override def children = List(child)

  override def inhibited = _inhibitedByWhen || child.inhibited

  override def toString = "'PrefixOperatorNode:" + operatorToken + inhibitedFlagForAstDump

  override def doWrite(sw: StatementWriter) = {
    sw.write("(")
    sw.write(operatorToken)
    if (newLineAfterOperator)
      sw.nextLine
    child.write(sw)
    sw.write(")")
  }
}

class LeftOuterJoinNode(left: ExpressionNode, right: ExpressionNode)
    extends BinaryOperatorNode(left, right, "left", false) {

  override def doWrite(sw: StatementWriter) = {}

  override def toString = "'LeftOuterJoin"
}

class FullOuterJoinNode(left: ExpressionNode, right: ExpressionNode)
    extends BinaryOperatorNode(left, right, "full", false) {
  override def toString = "'FullOuterJoin"
}

trait UniqueIdInAliaseRequired {
  var uniqueId: Option[Int] = None
}

trait QueryableExpressionNode extends ExpressionNode with UniqueIdInAliaseRequired {

  private[this] var _inhibited = false

  override def inhibited = _inhibited

  def inhibited_=(b: Boolean) = _inhibited = b

  /**
   * When the join syntax is used, isMemberOfJoinList is true if this instance is not in the from clause
   * but a 'join element'. 
   */
  def isMemberOfJoinList = joinKind.isDefined

  // new join syntax
  var joinKind: Option[(String, String)] = None

  def isOuterJoined =
    joinKind.isDefined && joinKind.get._2 == "outer"

  var joinExpression: Option[LogicalBoolean] = None

  // this 'old' join syntax will become deprecated :
  var outerJoinExpression: Option[OuterJoinExpression] = None

  var isRightJoined = false

  def isChild(q: QueryableExpressionNode): Boolean

  def owns(aSample: AnyRef): Boolean

  def alias: String

  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements): SelectElement

  def getOrCreateAllSelectElements(forScope: QueryExpressionElements): Iterable[SelectElement]

  def dumpAst = {
    val sb = new java.lang.StringBuilder
    visitDescendants { (n, parent, d: Int) =>
      val c = 4 * d
      for (i <- 1 to c) sb.append(' ')
      sb.append(n)
      sb.append("\n")
    }
    sb.toString
  }
}

class OrderByArg(val e: ExpressionNode) {

  private[this] var _ascending = true

  private[squeryl] def isAscending = _ascending

  def asc = {
    _ascending = true
    this
  }

  def desc = {
    _ascending = false
    this
  }
}

class OrderByExpression(a: OrderByArg) extends ExpressionNode {

  private def e = a.e

  override def inhibited = _inhibitedByWhen || e.inhibited

  def doWrite(sw: StatementWriter) = {
    e.write(sw)
    if (a.isAscending)
      sw.write(" Asc")
    else
      sw.write(" Desc")
  }

  override def children = List(e)

  def inverse = {

    val aCopy = new OrderByArg(a.e)

    if (aCopy.isAscending)
      aCopy.desc
    else
      aCopy.asc

    new OrderByExpression(aCopy)
  }
}

/**
 * Update, delete and insert statement are not built with AST nodes,
 * (for example Table[].update), although some portions of these statements
 * (where clauses are sometimes built with it.
 * The StatisticsListener needs to view every expression call as an AST,
 * which is the reason for this class.
 * AST are meant to be "non rendered", i.e. agnostic to specific DatabaseAdapter,
 * this DummyExpressionHolder is an exception.  
 * TODO: unify expression building to be completely AST based.
 */
class DummyExpressionHolder(val renderedExpression: String) extends ExpressionNode {

  def doWrite(sw: StatementWriter) =
    sw.write(renderedExpression)
}

class RightHandSideOfIn[A](val ast: ExpressionNode, val isIn: Option[Boolean] = None) extends ExpressionNode {
  def toIn = new RightHandSideOfIn[A](ast, Some(true))
  def toNotIn = new RightHandSideOfIn[A](ast, Some(false))

  override def children = List(ast)

  override def inhibited =
    super.inhibited ||
      (isConstantEmptyList && // not in Empty is always true, so we remove the condition
        (!isIn.get))

  def isConstantEmptyList: Boolean = ast match {
    case a: ConstantExpressionNodeList[_] =>
      a.isEmpty
    case a: ListExpressionNode =>
      a.children.isEmpty
    case _ =>
      false
  }

  override def doWrite(sw: StatementWriter) =
    if (isConstantEmptyList && isIn.get)
      sw.write("1 = 0") // in Empty is always false
    else {
      ast.doWrite(sw)
    }
}

class UnionExpressionNode(val kind: String, val ast: ExpressionNode) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = {
    sw.write(kind)
    sw.nextLine
    sw.write("(")
    sw.nextLine
    sw.indent(1)
    ast.write(sw)
    sw.unindent(1)
    sw.write(")")
    sw.nextLine
  }

  override def toString = {
    s"'UnionExpressionNode[with${kind}]"
  }

  override def children =
    List(ast)
}

class QueryValueExpressionNode[A1, T1](val ast: ExpressionNode, override val mapper: OutMapper[A1])
    extends TypedExpression[A1, T1] {
  def doWrite(sw: StatementWriter) = {
    ast.write(sw)
  }

  override def toString = {
    "'QueryValueExpressionNode"
  }

  override def children =
    List(ast)
}
