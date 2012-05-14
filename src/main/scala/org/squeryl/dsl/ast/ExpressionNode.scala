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


import collection.mutable.ArrayBuffer
import org.squeryl.internals._
import org.squeryl.dsl._
import org.squeryl.{Query, KeyedEntity, Schema, Session}
import javax.management.RuntimeErrorException
import java.sql.ResultSet

trait ExpressionNode {

  var parent: Option[ExpressionNode] = None

  def id = Integer.toHexString(System.identityHashCode(this))

  def inhibited = _inhibitedByWhen
  
  def inhibitedFlagForAstDump =
    if(inhibited) "!" else ""

  def write(sw: StatementWriter) =
    if(!inhibited)
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
          n: ExpressionNode, parent: Option[ExpressionNode], depth: Int,
          visitor: (ExpressionNode,Option[ExpressionNode],Int) => Unit): Unit = {
    visitor(n, parent, depth)
    n.children.foreach(child => _visitDescendants(child, Some(n), depth + 1, visitor))
  }


  private def _filterDescendants(n: ExpressionNode, ab: ArrayBuffer[ExpressionNode], predicate: (ExpressionNode) => Boolean): Iterable[ExpressionNode] = {
    if(predicate(n))
      ab.append(n)
    n.children.foreach(child => _filterDescendants(child, ab, predicate))
    ab
  }

  def filterDescendants(predicate: (ExpressionNode) => Boolean) =
    _filterDescendants(this, new ArrayBuffer[ExpressionNode], predicate)


  def filterDescendantsOfType[T](implicit manifest: Manifest[T]) =
    _filterDescendants(
      this,
      new ArrayBuffer[ExpressionNode],
      (n:ExpressionNode)=> manifest.erasure.isAssignableFrom(n.getClass)
    ).asInstanceOf[Iterable[T]]

  /**
   * visitor's args are :
   *  -the visited node,
   *  -it's parent
   *  -it's depth
   */
  def visitDescendants(visitor: (ExpressionNode,Option[ExpressionNode],Int) => Unit) =
    _visitDescendants(this, None, 0, visitor)

  protected var _inhibitedByWhen = false

  def inhibitWhen(inhibited: Boolean): this.type = {
    _inhibitedByWhen = inhibited
    this
  }

  def ? : this.type = {
    if(! this.isInstanceOf[ConstantTypedExpression[_,_]])
      org.squeryl.internals.Utils.throwError("the '?' operator (shorthand for 'p.inhibitWhen(p == None))' can only be used on a constant query argument")

    val c = this.asInstanceOf[ConstantTypedExpression[_,_]]

    inhibitWhen(c.value == None)
  }
}


trait ListExpressionNode extends ExpressionNode {

  def quotesElement = false
  
  def isEmpty: Boolean
}

class EqualityExpression(override val left: TypedExpression[_,_], override val right: TypedExpression[_,_]) extends BinaryOperatorNodeLogicalBoolean(left, right, "=") {
  
  override def doWrite(sw: StatementWriter) =     
    right match {
      case c: ConstantTypedExpression[_,_] => 
        if(c.value == None) {
          left.write(sw)
          sw.write(" is null")
        }
        else super.doWrite(sw)
      case _ => super.doWrite(sw)
    }
  
}

class InclusionOperator(left: ExpressionNode, right: RightHandSideOfIn[_]) extends BinaryOperatorNodeLogicalBoolean(left, right, "in", true) {

  override def doWrite(sw: StatementWriter) =
    if(right.isConstantEmptyList)
      sw.write("(1 = 0)")
    else
      super.doWrite(sw)
}

class ExclusionOperator(left: ExpressionNode, right: RightHandSideOfIn[_]) extends BinaryOperatorNodeLogicalBoolean(left, right, "not in", true)

class BinaryOperatorNodeLogicalBoolean(left: ExpressionNode, right: ExpressionNode, op: String, rightArgInParent: Boolean = false)
  extends BinaryOperatorNode(left,right, op) with LogicalBoolean {

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
    val nonInh = children.filter(c => ! c.inhibited).iterator

    sw.write("(")
    nonInh.next.write(sw)
    sw.write(" ")
    if(nonInh.hasNext) {
      sw.write(operatorToken)
      if(newLineAfterOperator)
        sw.nextLine
      sw.write(" ")

      if(rightArgInParent)
        sw.write("(")

      nonInh.next.write(sw)
      
      if(rightArgInParent)
        sw.write(")")      
    }
    sw.write(")")
  }
}

class ExistsExpression(val ast: ExpressionNode, val opType: String)
  extends PrefixOperatorNode(ast, opType, false) with LogicalBoolean with NestedExpression

class BetweenExpression(first: ExpressionNode, second: ExpressionNode, third: ExpressionNode)
  extends TernaryOperatorNode(first, second, third, "between") with LogicalBoolean {

  override def doWrite(sw: StatementWriter) = {
    first.write(sw)
    sw.write(" between ")
    second.write(sw)
    sw.write(" and ")
    third.write(sw)
  }
}

class TernaryOperatorNode(val first: ExpressionNode, val second: ExpressionNode, val third: ExpressionNode, op: String)
  extends FunctionNode(op, Seq(first, second, third)) with LogicalBoolean {

  override def inhibited =
    _inhibitedByWhen || first.inhibited || second.inhibited || third.inhibited
}

trait LogicalBoolean extends ExpressionNode  {

  def and(b: LogicalBoolean) = new BinaryOperatorNodeLogicalBoolean(this, b, "and")
  def or(b: LogicalBoolean) = new BinaryOperatorNodeLogicalBoolean(this, b, "or")
}


class UpdateAssignment(val left: FieldMetaData, val right: ExpressionNode)

trait BaseColumnAttributeAssignment {

  def clearColumnAttributes: Unit

  def isIdFieldOfKeyedEntity: Boolean

  def isIdFieldOfKeyedEntityWithoutUniquenessConstraint =
    isIdFieldOfKeyedEntity && ! (columnAttributes.exists(_.isInstanceOf[PrimaryKey]) || columnAttributes.exists(_.isInstanceOf[Unique]))

  def columnAttributes: Seq[ColumnAttribute]

  def hasAttribute[A <: ColumnAttribute](implicit m: Manifest[A]) =
    findAttribute[A](m) != None

  def findAttribute[A <: ColumnAttribute](implicit m: Manifest[A]) =
    columnAttributes.find(ca => m.erasure.isAssignableFrom(ca.getClass))  
}

class ColumnGroupAttributeAssignment(cols: Seq[FieldMetaData], columnAttributes_ : Seq[ColumnAttribute])
  extends BaseColumnAttributeAssignment {

  private val _columnAttributes = new ArrayBuffer[ColumnAttribute]

  _columnAttributes.appendAll(columnAttributes_)

  def columnAttributes = _columnAttributes 

  def addAttribute(a: ColumnAttribute) =
    _columnAttributes.append(a)

  def clearColumnAttributes = columns.foreach(_._clearColumnAttributes)

  def columns: Seq[FieldMetaData] = cols

  def isIdFieldOfKeyedEntity = false

  def name:Option[String] = None
}

class CompositeKeyAttributeAssignment(val group: CompositeKey, _columnAttributes: Seq[ColumnAttribute])
  extends ColumnGroupAttributeAssignment(group._fields, _columnAttributes) {

  override def isIdFieldOfKeyedEntity = {
    val fmdHead = group._fields.head
    classOf[KeyedEntity[Any]].isAssignableFrom(fmdHead.parentMetaData.clasz) &&
    group._propertyName == "id"
  }

  assert(group._propertyName != None)

  override def name:Option[String] = group._propertyName
}

class ColumnAttributeAssignment(val left: FieldMetaData, val columnAttributes: Seq[ColumnAttribute])
  extends BaseColumnAttributeAssignment {

  def clearColumnAttributes = left._clearColumnAttributes

  def isIdFieldOfKeyedEntity = left.isIdFieldOfKeyedEntity 
}

class DefaultValueAssignment(val left: FieldMetaData, val value: TypedExpression[_,_])
  extends BaseColumnAttributeAssignment {

  def isIdFieldOfKeyedEntity = left.isIdFieldOfKeyedEntity

  def clearColumnAttributes = left._clearColumnAttributes

  def columnAttributes = Nil
}


class TokenExpressionNode(val token: String) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = sw.write(token)
}


class InputOnlyConstantExpressionNode(v: Any) extends ConstantTypedExpression[Any,Any](v, NoOpOutMapper, v.asInstanceOf[AnyRef])

class ConstantTypedExpression[A1,T1](val value: A1, override val mapper: OutMapper[A1], nativeJdbcValue: AnyRef) extends TypedExpression[A1,T1] {

  private def needsQuote = value.isInstanceOf[String]

  def doWrite(sw: StatementWriter) = {
    if(sw.isForDisplay) {
      if(value == null)
        sw.write("null")
      else if(needsQuote) {
        sw.write("'")
        sw.write(value.toString)
        sw.write("'")
      }
      else
        sw.write(value.toString)
    }
    else {
      sw.write("?")
      sw.addParam(nativeJdbcValue)
    }
  }
  override def toString = 'ConstantTypedExpression + ":" + value
}

class ConstantExpressionNodeList[T](val value: Traversable[T]) extends ExpressionNode {

  def isEmpty =
    value == Nil

  def doWrite(sw: StatementWriter) =
    if(sw.isForDisplay)
      sw.write(this.value.map(e=>"'" +e+"'").mkString(","))
    else {
      sw.write(this.value.toSeq.map(z => "?").mkString(","))
      this.value.foreach(z => sw.addParam(z.asInstanceOf[AnyRef]))
    }
}

class FunctionNode(val name: String, val args: Seq[ExpressionNode]) extends ExpressionNode {
        
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

  override def doWrite(sw: StatementWriter)= e.doWrite((sw))

  override def children = e.children
}

class BinaryOperatorNode
 (val left: ExpressionNode, val right: ExpressionNode, val operatorToken: String, val newLineAfterOperator: Boolean = false)
  extends ExpressionNode {

  override def children = List(left, right)

  override def inhibited =
     _inhibitedByWhen || left.inhibited || right.inhibited

  override def toString =
    'BinaryOperatorNode + ":" + operatorToken + inhibitedFlagForAstDump
  
  def doWrite(sw: StatementWriter) = {
    sw.write("(")
    left.write(sw)
    sw.write(" ")
    sw.write(operatorToken)
    if(newLineAfterOperator)
      sw.nextLine
    sw.write(" ")
    right.write(sw)
    sw.write(")")
  }
}

class PrefixOperatorNode
 (val child: ExpressionNode, val operatorToken: String, val newLineAfterOperator: Boolean = false)
  extends ExpressionNode {

  override def children = List(child)

  override def inhibited = child.inhibited

  override def toString = 'PrefixOperatorNode + ":" + operatorToken + inhibitedFlagForAstDump

  override def doWrite(sw: StatementWriter) = {
    sw.write("(")
    sw.write(operatorToken)
    if(newLineAfterOperator)
      sw.nextLine
    child.write(sw)
    sw.write(")")
  }
}

class LeftOuterJoinNode
 (left: ExpressionNode, right: ExpressionNode)
  extends BinaryOperatorNode(left,right, "left", false) {

  override def doWrite(sw: StatementWriter) = {}
  
  override def toString = 'LeftOuterJoin + ""  
}

class FullOuterJoinNode(left: ExpressionNode, right: ExpressionNode) extends BinaryOperatorNode(left, right, "full", false) {
  override def toString = 'FullOuterJoin + ""
}

trait UniqueIdInAliaseRequired  {
  var uniqueId: Option[Int] = None 
}

trait QueryableExpressionNode extends ExpressionNode with UniqueIdInAliaseRequired {

  private var _inhibited = false

  override def inhibited = _inhibited

  def inhibited_=(b: Boolean) = _inhibited = b

  /**
   * When the join syntax is used, isMemberOfJoinList is true if this instance is not in the from clause
   * but a 'join element'. 
   */
  def isMemberOfJoinList = joinKind != None

  // new join syntax
  var joinKind: Option[(String,String)] = None

  def isOuterJoined =
    joinKind != None && joinKind.get._2 == "outer"

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
    val sb = new StringBuffer
    visitDescendants {(n,parent,d:Int) =>
      val c = 4 * d
      for(i <- 1 to c) sb.append(' ')
      sb.append(n)
      sb.append("\n")
    }
    sb.toString
  }  
}

class OrderByArg(val e: ExpressionNode) {

  private var _ascending = true

  private [squeryl] def isAscending = _ascending

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
  
  override def inhibited = e.inhibited

  def doWrite(sw: StatementWriter) = {
    e.write(sw)
    if(a.isAscending)
      sw.write(" Asc")
    else
      sw.write(" Desc")
  }

  override def children = List(e)

  def inverse = {

    val aCopy = new OrderByArg(a.e)

    if(aCopy.isAscending)
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

class RightHandSideOfIn[A](val ast: ExpressionNode, val isIn: Option[Boolean] = None)
    extends ExpressionNode with NestedExpression {
  def toIn = new RightHandSideOfIn[A](ast, Some(true))
  def toNotIn = new RightHandSideOfIn[A](ast, Some(false))

  override def children = List(ast)

  override def inhibited =
    super.inhibited ||
    (isConstantEmptyList && // not in Empty is always true, so we remove the condition
      (! isIn.get))

  def isConstantEmptyList =
    if(ast.isInstanceOf[ConstantExpressionNodeList[_]]) {
      ast.asInstanceOf[ConstantExpressionNodeList[_]].isEmpty
    }
    else false

  override def doWrite(sw: StatementWriter) =
    if(isConstantEmptyList && isIn.get)
      sw.write("1 = 0") // in Empty is always false
    else {
      ast.doWrite(sw)
    }
}

trait NestedExpression {
  self: ExpressionNode =>

  private [squeryl] def propagateOuterScope(query:QueryExpressionNode[_]) {
    visitDescendants( (node, parent, depth) =>
      node match {
        case e:ExportedSelectElement if e.needsOuterScope => e.outerScopes = query :: e.outerScopes
        case s:SelectElementReference[_,_] => s.delegateAtUseSite match {
          case e:ExportedSelectElement if e.needsOuterScope => e.outerScopes = query :: e.outerScopes
          case _ =>
        }
        case _ =>
      }
    )
  }
}

class UnionExpressionNode(val kind: String, val ast: ExpressionNode) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = {
    sw.write(kind)
    sw.nextLine
    ast.write(sw)
  }

  override def toString = {
    'UnionExpressionNode + "[with " + kind  + "]"
  }

  override def children =
    List(ast)
}
