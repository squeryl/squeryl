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

class QueryExpressionNode[R](val _query: AbstractQuery[R],
                             _queryYield:QueryYield[R],
                             val subQueries: Iterable[QueryableExpressionNode],
                             val views: Iterable[ViewExpressionNode[_]])
  extends QueryExpressionElements
    with QueryableExpressionNode {

  private [squeryl] def cteRoot: Option[QueryExpressionElements] = {
    def loop(current: Option[ExpressionNode]): Option[QueryExpressionElements] = {
      current.flatMap { c =>
        c.isInstanceOf[QueryExpressionNode[_]] match {
          case true => c.asInstanceOf[QueryExpressionNode[_]]
            .commonTableExpressions
            .find(sameRoot_?)
            .orElse(loop(c.parent))
          case false => loop(c.parent)
        }
      }
    }
    loop(parent)
  }

  private [squeryl] def sameRoot_?(e: QueryExpressionNode[_]) =
    _query.root.isDefined && _query.root == e._query.root

  def tableExpressions: Iterable[QueryableExpressionNode] = 
    List(views.filter(v => ! v.inhibited),
         subQueries.filter(v => ! v.inhibited)).flatten

  def isJoinForm = _queryYield.joinExpressions != Nil

  val (whereClause, havingClause, groupByClause, orderByClause, ctes) =
     _queryYield.queryElements

  val commonTableExpressions = ctes.map { q =>
    if (! q.ast.isInstanceOf[QueryExpressionNode[_]]) {
      Utils.throwError("A common table expression AST must be a QueryExpressionNode, not a " +
        q.getClass.getSimpleName)
    }
    q.ast.asInstanceOf[QueryExpressionNode[_]]
  }.toList

  private val unionClauses =
      _query.unions map (kindAndQ => new UnionExpressionNode(kindAndQ._1, kindAndQ._2.ast))

  private var _selectList: Iterable[SelectElement] = Iterable.empty

  private var _sample: Option[AnyRef] = None

  private def _isPrimitiveType(o: AnyRef) = // AnyRef can not be primitive.
    List("java.lang.Boolean",
      "java.lang.Character",
      "java.lang.Byte",
      "java.lang.Short",
      "java.lang.Integer",
      "java.lang.Long",
      "java.lang.Float",
      "java.lang.Double",
      "java.lang.Void").contains(o.getClass.getName)

  def isUseableAsSubquery: Boolean =
    _sample match {
      case None => throw new IllegalStateException("method cannot be called before initialization")
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
  
  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements) = throw new UnsupportedOperationException("implement me")

  override def toString = {
    val sb = new java.lang.StringBuilder
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
      commonTableExpressions,
      views.toList,
      subQueries.toList,
      tableExpressions.flatMap(_.joinExpression).toList,
      whereClause.toList,
      groupByClause.toList,
      havingClause.toList,
      orderByClause.toList,
      unionClauses
    ).flatten

  def isChild(q: QueryableExpressionNode):Boolean =
    views.exists(n => n == q)

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

      if (commonTableExpressions.nonEmpty) {
        relabelCtes()
      }
    }
  }

  private def relabelCtes(): Unit = {
    val cteNodes = commonTableExpressions.map { cte =>
      (cte, collectAliasedNodes(cte))
    }

    for {
      ref <- collectCteRefs()
      (cte, src) <- cteNodes.find(_._1.sameRoot_?(ref))
    } {
      copyUniqueIds(src, collectAliasedNodes(ref))
    }
  }

  private def collectCteRefs(): List[QueryExpressionNode[_]] = {
    val buf = new collection.mutable.ArrayBuffer[QueryExpressionNode[_]]()
    visitDescendants((node, parent, i) => {
      if (! commonTableExpressions.contains(node) &&
        node.isInstanceOf[QueryExpressionNode[_]] &&
        commonTableExpressions.exists(
          e => e.sameRoot_?(node.asInstanceOf[QueryExpressionNode[_]]))) {

        buf += node.asInstanceOf[QueryExpressionNode[_]]
      }
    })

    buf.toList
  }

  private def collectAliasedNodes(e: ExpressionNode): List[UniqueIdInAliaseRequired] = {
    val buf = new collection.mutable.ArrayBuffer[UniqueIdInAliaseRequired]()
    e.visitDescendants((node, parent, i) => {
      if ((node ne e) && node.isInstanceOf[UniqueIdInAliaseRequired]) {
        buf += node.asInstanceOf[UniqueIdInAliaseRequired]
      }
    })
    buf.toList
  }

  private def copyUniqueIds(src: List[UniqueIdInAliaseRequired], dest: List[UniqueIdInAliaseRequired]): Unit = {
    src.zip(dest).collect { case (e1, e2) => e2.uniqueId = e1.uniqueId }
  }

  def selectList: Iterable[SelectElement] = _selectList

  def doWrite(sw: StatementWriter) = {
    def writeCompleteQuery = {
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

    if (sw.databaseAdapter.supportsCommonTableExpressions) {
      cteRoot.map { r =>
        sw.databaseAdapter.writeCteReference(sw, r)
      }.getOrElse(writeCompleteQuery)
    } else {
      writeCompleteQuery
    }
  }
}

