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
import org.squeryl.{KeyedEntity, Schema, Session}

trait ExpressionNode {

  var parent: Option[ExpressionNode] = None

  def id = Integer.toHexString(System.identityHashCode(this))

  def inhibited = false
  
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
}


trait ListExpressionNode extends ExpressionNode {

  def quotesElement = false
  
  def isEmpty: Boolean
}

trait ListNumerical extends ListExpressionNode


trait ListDouble extends ListNumerical
trait ListBigDecimal extends ListNumerical
trait ListFloat  extends ListNumerical
trait ListInt extends ListNumerical
trait ListLong extends ListNumerical
trait ListDate extends ListExpressionNode

trait ListString extends ListExpressionNode {
  override def quotesElement = true
}

class EqualityExpression(override val left: TypedExpressionNode[_], override val right: TypedExpressionNode[_]) extends BinaryOperatorNodeLogicalBoolean(left, right, "=")

class InListExpression(left: ExpressionNode, right: ListExpressionNode, inclusion: Boolean) extends BinaryOperatorNodeLogicalBoolean(left, right, if(inclusion) "in" else "not in") {

  override def inhibited =
    if(right.isEmpty)
      (! inclusion)
    else
      super.inhibited

  override def doWrite(sw: StatementWriter) =
    if(inclusion  && right.isEmpty)
      sw.write("(1 = 0)")
    else
      super.doWrite(sw)
}


class BinaryOperatorNodeLogicalBoolean(left: ExpressionNode, right: ExpressionNode, op: String)
  extends BinaryOperatorNode(left,right, op) with LogicalBoolean {

  override def inhibited =
    if(left.isInstanceOf[LogicalBoolean])
      left.inhibited && right.inhibited
    else
      left.inhibited || right.inhibited
  
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
      nonInh.next.write(sw)
    }
    sw.write(")")
  }
}

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
  extends FunctionNode(op, None, List(first, second, third)) with LogicalBoolean {

  override def inhibited =
    first.inhibited || second.inhibited || third.inhibited
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

class DefaultValueAssignment(val left: FieldMetaData, val value: TypedExpressionNode[_])
  extends BaseColumnAttributeAssignment {

  def isIdFieldOfKeyedEntity = left.isIdFieldOfKeyedEntity

  def clearColumnAttributes = left._clearColumnAttributes

  def columnAttributes = Nil
}


trait TypedExpressionNode[T] extends ExpressionNode {

  def sample:T = mapper.sample

  def mapper: OutMapper[T]

  def :=[B <% TypedExpressionNode[T]] (b: B) =
    new UpdateAssignment(_fieldMetaData, b : TypedExpressionNode[T])

  def defaultsTo[B <% TypedExpressionNode[T]](value: B) /*(implicit restrictUsageWithinSchema: Schema) */ =
    new DefaultValueAssignment(_fieldMetaData, value : TypedExpressionNode[T])

  /**
   * TODO: make safer with compiler plugin
   * Not type safe ! a TypedExpressionNode[T] might not be a SelectElementReference[_] that refers to a FieldSelectElement...   
   */
  private [squeryl] def _fieldMetaData = {
    val ser =
      try {
        this.asInstanceOf[SelectElementReference[_]]
      }
      catch { // TODO: validate this at compile time with a scalac plugin
        case e:ClassCastException => {
            throw new RuntimeException("left side of assignment '" + Utils.failSafeString(this.toString)+ "' is invalid, make sure statement uses *only* closure argument.", e)
        }
      }

    val fmd =
      try {
        ser.selectElement.asInstanceOf[FieldSelectElement].fieldMataData
      }
      catch { // TODO: validate this at compile time with a scalac plugin
        case e:ClassCastException => {
          throw new RuntimeException("left side of assignment '" + Utils.failSafeString(this.toString)+ "' is invalid, make sure statement uses *only* closure argument.", e)
        }
      }
    fmd
  }

  private var _inhibitedByWhen = false

  override def inhibited =
    super.inhibited || _inhibitedByWhen

  def inhibitWhen(inhibited: Boolean): this.type = {
    _inhibitedByWhen = inhibited
    this
  }

  def ? : this.type = {
    if(! this.isInstanceOf[ConstantExpressionNode[_]])
      error("the '?' operator (shorthand for 'p.inhibitWhen(p == None))' can only be used on a constant query argument")

    val c = this.asInstanceOf[ConstantExpressionNode[_]]

    inhibitWhen(c.value == None)
  }  
}

class TokenExpressionNode(val token: String) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = sw.write(token)
}

class ConstantExpressionNode[T](val value: T) extends ExpressionNode {

  //def this(v:T) = this(v, false)

  private def needsQuote = value.isInstanceOf[String]

  def mapper: OutMapper[T] = error("outMapper should not be used on " + 'ConstantExpressionNode)

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
      sw.addParam(value.asInstanceOf[AnyRef])
    }
  }
  override def toString = 'ConstantExpressionNode + ":" + value
}

class ConstantExpressionNodeList[T](val value: Traversable[T]) extends ExpressionNode with ListExpressionNode {

  def isEmpty =
    value == Nil

  def doWrite(sw: StatementWriter) =
    if(quotesElement)
      sw.write(this.value.map(e=>"'" +e+"'").mkString("(",",",")"))
    else
      sw.write(this.value.mkString("(",",",")"))
}

class FunctionNode[A](val name: String, _mapper : Option[OutMapper[A]], val args: Iterable[ExpressionNode]) extends ExpressionNode {

  def this(name: String, args: ExpressionNode*) = this(name, None, args)

  def mapper: OutMapper[A] = _mapper.getOrElse(error("no mapper available"))

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
    left.inhibited || right.inhibited 

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

  def isOuterJoinedDEPRECATED = outerJoinExpression != None

  var isRightJoined = false
  
  def dumpOuterJoinInfoForAst(sb: StringBuffer) =
    if(isOuterJoinedDEPRECATED) {
      val oje = outerJoinExpression.get
      sb.append(oje.leftRightOrFull)
      sb.append("OuterJoin(")
      sb.append(oje.matchExpression.writeToString)
      sb.append(")")
    }
  
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