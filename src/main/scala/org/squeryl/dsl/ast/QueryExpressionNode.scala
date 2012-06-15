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

import org.squeryl.internals._
import org.squeryl.dsl.{QueryYield, AbstractQuery}

class QueryExpressionNode[R](_query: AbstractQuery[R],
                             _queryYield:QueryYield[R],
                             val subQueries: Iterable[QueryableExpressionNode],
                             val views: Iterable[ViewExpressionNode[_]])
  extends QueryExpressionElements
    with QueryableExpressionNode {

  def tableExpressions: Iterable[QueryableExpressionNode] = 
    List(views.filter(v => ! v.inhibited),
         subQueries.filter(v => ! v.inhibited)).flatten

  def isJoinForm = _queryYield.joinExpressions != Nil

  val (whereClause, havingClause, groupByClause, orderByClause) =
     _queryYield.queryElements

  private val unionClauses =
      _query.unions map (kindAndQ => new UnionExpressionNode(kindAndQ._1, kindAndQ._2.ast))

  private var _selectList: Iterable[SelectElement] = Iterable.empty

  private var _sample: Option[AnyRef] = None

  private def _isPrimitiveType(o: AnyRef) =
    o.getClass.isPrimitive

  def isUseableAsSubquery: Boolean =
    _sample match {
      case None => org.squeryl.internals.Utils.throwError("method cannot be called before initialization")
      case Some(p:Product) =>
        if(p.getClass.getName.startsWith("scala.Tuple")) {
          val z = (for(i <- 0 to (p.productArity - 1)) yield p.productElement(i))
          ! z.exists(o => _isPrimitiveType(o.asInstanceOf[AnyRef]))
        }
        else
          true
      case Some(a:AnyRef) => ! _isPrimitiveType(a)
    }


  def sample:AnyRef = _sample.get

  def owns(aSample: AnyRef) = 
    _sample != None && _sample.get.eq(aSample)
  
  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements) = org.squeryl.internals.Utils.throwError("implement me")

  override def toString = {
    val sb = new StringBuffer
    sb.append('QueryExpressionNode + "[")
    if(_query.isRoot)
      sb.append("root:")
    sb.append(id)
    sb.append("]")
    sb.append(":rsm="+_query.resultSetMapper)
    sb.toString
  }

  override def children =
    List(
      selectList.toList,
      views.toList,
      subQueries.toList,
      tableExpressions.filter(e=> e.joinExpression != None).map(_.joinExpression.get).toList,  
      whereClause.toList,
      groupByClause.toList,
      havingClause.toList,
      orderByClause.toList,
      unionClauses
    ).flatten

  def isChild(q: QueryableExpressionNode):Boolean =
    views.find(n => n == q) != None

  def selectDistinct = _query.selectDistinct

  def isForUpdate = _query.isForUpdate

  def page = _query.page

  def unionIsForUpdate = _query.unionIsForUpdate

  def unionPage = _query.unionPage

  def alias = "q" + uniqueId.get

  def getOrCreateAllSelectElements(forScope: QueryExpressionElements): Iterable[SelectElement] = {
    _selectList.map(se => new ExportedSelectElement(se))
  }

  private def hasUnionQueryOptions = unionIsForUpdate || unionPage.isDefined

  def setOutExpressionNodesAndSample(sl: Iterable[SelectElement], s: AnyRef) = {
    _selectList = sl
    _sample = Some(s)

    if(_query.isRoot) {

      var jdbcIndex = 1
      for(oen <- selectList) {
        oen.prepareMapper(jdbcIndex)
        jdbcIndex += 1
      }

      var idGen = 0
      visitDescendants((node,parent,i) => {
        node.parent = parent

        if(node.isInstanceOf[UniqueIdInAliaseRequired]) {
          val nxn = node.asInstanceOf[UniqueIdInAliaseRequired]
          nxn.uniqueId = Some(idGen)
          idGen += 1
        }
      })
    }
  }

  def propagateOuterScope() {
    for {
      node <- children if ! node.isInstanceOf[UnionExpressionNode]
    } {
      node.filterDescendantsOfType[NestedExpression].foreach((n) => n.propagateOuterScope(this))
    }
  }

  def selectList: Iterable[SelectElement] = _selectList

  def doWrite(sw: StatementWriter) = {
    val isNotRoot = parent != None
    val isContainedInUnion = parent map (_.isInstanceOf[UnionExpressionNode]) getOrElse (false)

    if((isNotRoot && ! isContainedInUnion) || hasUnionQueryOptions) {
      sw.write("(")
      sw.indent(1)
    }

    if (! unionClauses.isEmpty) {
      sw.write("(")
      sw.nextLine
      sw.indent(1)
    }

    sw.databaseAdapter.writeQuery(this, sw)

    if (! unionClauses.isEmpty) {
      sw.unindent(1)
      sw.write(")")
      sw.nextLine
    }

    unionClauses.foreach { u =>
      u.write(sw)
    }

    if((isNotRoot && ! isContainedInUnion) || hasUnionQueryOptions) {
      sw.unindent(1)
      sw.write(") ")
    }

    if (hasUnionQueryOptions) {
      sw.databaseAdapter.writeUnionQueryOptions(this, sw)
    }
  }
}

