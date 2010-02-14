package org.squeryl.dsl.ast


import collection.mutable.ArrayBuffer
import org.squeryl.internals._
import org.squeryl.Session

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
}

trait ListNumerical extends ListExpressionNode


trait ListDouble extends ListNumerical
trait ListFloat  extends ListNumerical
trait ListInt extends ListNumerical
trait ListLong extends ListNumerical
trait ListDate extends ListExpressionNode

trait ListString extends ListExpressionNode {
  override def quotesElement = true
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

trait LogicalBoolean extends ExpressionNode  {

  def and(b: LogicalBoolean) = new BinaryOperatorNodeLogicalBoolean(this, b, "and")
  def or(b: LogicalBoolean) = new BinaryOperatorNodeLogicalBoolean(this, b, "or")
}


class UpdateAssignment(val left: ExpressionNode, val right: ExpressionNode)

trait TypedExpressionNode[T] extends ExpressionNode {

  def sample:T = mapper.sample

  def mapper: OutMapper[T]

  def :=[B <% TypedExpressionNode[T]] (b: B) = new UpdateAssignment(this, b : TypedExpressionNode[T])
}

class TokenExpressionNode(val token: String) extends ExpressionNode {
  def doWrite(sw: StatementWriter) = sw.write(token)
}

class ConstantExpressionNode[T](val value: T, needsQuote: Boolean) extends ExpressionNode {

  def this(v:T) = this(v, false)

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

class ConstantExpressionNodeList[T](val value: List[T]) extends ExpressionNode with ListExpressionNode {
  
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
  //self: ExpressionNode =>

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
   * outerJoinColumns is None if not an outer join, args are (left col : SelectElementReference, right col : SelectElementReference, outer Join kind: String ("left" or "full")) 
   */
  var outerJoinColumns: Option[(SelectElementReference[_],SelectElementReference[_], String)] = None

  var fullOuterJoinMatchColumn: Option[SelectElementReference[_]] = None

  def isOptionalInOuterJoin =
    outerJoinColumns != None || fullOuterJoinMatchColumn != None
  
  def dumpOuterJoinInfoForAst(sb: StringBuffer) = {
    if(outerJoinColumns != None) {
      sb.append("OuterJoin(")
      sb.append(outerJoinColumns.get._1)
      sb.append(" ~> ")
      sb.append(outerJoinColumns.get._2)
      sb.append(")")
    }
    if(fullOuterJoinMatchColumn != None) {
      sb.append("FullOuterJoin(")
      sb.append(fullOuterJoinMatchColumn.get)
      sb.append(")")
    }
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

class OrderByExpression(a: OrderByArg) extends ExpressionNode { //with TypedExpressionNode[_] {

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
  
}
