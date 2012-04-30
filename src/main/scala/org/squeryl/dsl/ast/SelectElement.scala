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
import java.sql.ResultSet
import org.squeryl.{PgOptimistic, Session}
import org.squeryl.dsl.TypedExpression
import scala.annotation.tailrec

/**
 * SelectElement are elements of a select list, for example they are a,b, and c in :
 *
 * select a,b,c from T
 *
 * they are either
 *  ValueSelectElement for composite expressions, i.e. select (x / 2) * y as Z from ....
 *  TupleSelectElement for group by or compute elements (TODO: document group by/compute)
 *  FieldSelectElement for table columns (that map to fields)
 *
 *  ExportSelectElement for a select element that refers to a SelectElement of an inner or outer query.
 *
 * SelectElementReference are nodes in any clause other than select (where, having, composite expression, order by, etc)
 *  that refer to a SelectElement  
 */
trait SelectElement extends ExpressionNode {
  outer =>
  /**
   * <pre>
   * In the following select :
   *
   *   select t.x from t
   *
   *  t.x is a select element and t is it's origin
   *
   * Here q.z1 is a SelectElement who's origin is t
   *
   *   select q.z1
   *   from
   *     (select t.x as z1 from t) q
   *
   * </pre>
   */  
  def origin: QueryableExpressionNode

  def parentQueryable = parent.get.asInstanceOf[QueryableExpressionNode]  

  def resultSetMapper: ResultSetMapper

  def alias: String

  def aliasSegment: String =
    alias

  def actualSelectElement: SelectElement = this

  /**
   * Update, Insert, and Delete statements are always at the root of an AST, so they
   * are never aliased, but then can have sub queries, ex.: update ... where x in (subquery).
   * Name clashes are impossible since SelectElements of query are always aliased.
   *
   */
  def inhibitAliasOnSelectElementReference: Boolean = {

    def shouldInhibit(e: ExpressionNode): Boolean =
      e.parent map ({ p =>
        if(p.isInstanceOf[QueryExpressionElements])
          p.asInstanceOf[QueryExpressionElements].inhibitAliasOnSelectElementReference
        else
          shouldInhibit(p)
      }) getOrElse true

    shouldInhibit(origin)
  }

  def realTableNamePrefix: Boolean = {

    def parent(e: ExpressionNode): ExpressionNode =
      (e.parent map (p => parent(p))) getOrElse e

    val p = parent(origin)
    p.isInstanceOf[UpdateStatement] || p.isInstanceOf[QueryExpressionElements]
  }

  def prepareColumnMapper(index: Int): Unit

  def prepareMapper(jdbcIndex: Int): Unit

  override def inhibited =
    origin.inhibited

  def isActive = _isActive

  protected [squeryl] var _isActive = false
  
  def expression: ExpressionNode

  /**
   * strictly for logging purposes, i.e. to display a more explicit AST
   */
  def typeOfExpressionToString: String

  override def children = List(expression)

  def doWrite(sw: StatementWriter) = {
    expression.write(sw)
    sw.write(" as ")
    sw.databaseAdapter.writeSelectElementAlias(this, sw)
  }
}

class TupleSelectElement
 (val origin: QueryExpressionNode[_], val expression: ExpressionNode, indexInTuple: Int, isGroupTuple: Boolean)
    extends SelectElement {

  def resultSetMapper: ResultSetMapper = throw new UnsupportedOperationException("refactor me")

  //TODO: normalize ?
  def alias =
    if(isGroupTuple)
      "g" + indexInTuple
    else
      "c" + indexInTuple


  var columnToTupleMapper: Option[ColumnToTupleMapper] = None

  def prepareColumnMapper(index: Int) = {}

  def typeOfExpressionToString: String =
    if(columnToTupleMapper == None)
      "unknown"
    else
      columnToTupleMapper.get.typeOfExpressionToString(indexInTuple)

  override def prepareMapper(jdbcIndex: Int) =
    if(columnToTupleMapper != None)
      columnToTupleMapper.get.activate(indexInTuple, jdbcIndex)

  override def toString =
    'TupleSelectElement + ":" + indexInTuple + ":" + writeToString
}

class FieldSelectElement
(val origin: ViewExpressionNode[_], val fieldMetaData: FieldMetaData, val resultSetMapper: ResultSetMapper)
  extends SelectElement with UniqueIdInAliaseRequired {

  def alias =
    if(inhibitAliasOnSelectElementReference)
      if(realTableNamePrefix)
        origin.view.name + "." + fieldMetaData.columnName
      else
        fieldMetaData.columnName
    else
      origin.alias + "." + fieldMetaData.columnName

  override def aliasSegment: String =
    Session.currentSession.databaseAdapter.fieldAlias(origin, this)
    //origin.alias + "_" + fieldMetaData.columnName
  
  val expression = new ExpressionNode {
    
    def doWrite(sw: StatementWriter) = {
      sw.write(sw.quoteName(alias))
      if (classOf[PgOptimistic].isAssignableFrom(origin.view.classOfT)
          && fieldMetaData.columnName == "ctid") {
        sw.write("::varchar")
      }
    }
  }

  def prepareColumnMapper(index: Int) =
    columnMapper = Some(new ColumnToFieldMapper(index, fieldMetaData, this))

  private var columnMapper: Option[ColumnToFieldMapper] = None

  def prepareMapper(jdbcIndex: Int) =
    if(columnMapper != None) {
      resultSetMapper.addColumnMapper(columnMapper.get)
      resultSetMapper.isActive = true
      _isActive = true
    }
  
  def typeOfExpressionToString =
    fieldMetaData.displayType
  
  override def toString =
    'FieldSelectElement + ":" +
       Utils.failSafeString(alias, fieldMetaData.nameOfProperty)
}

class ValueSelectElement
  (val expression: ExpressionNode, val resultSetMapper: ResultSetMapper, mapper: OutMapper[_], val origin: QueryExpressionNode[_])
     extends SelectElement with UniqueIdInAliaseRequired {

  def alias = "v" + uniqueId.get

  var yieldPusher: Option[YieldValuePusher] = None

  def prepareColumnMapper(index: Int) =
    yieldPusher = Some(new YieldValuePusher(index, this, mapper))  

  def typeOfExpressionToString =
    if(yieldPusher == None)
      "unknown"
    else
      yieldPusher.get.selectElement.typeOfExpressionToString
  
  override def prepareMapper(jdbcIndex: Int) =
    if(yieldPusher != None) {
      resultSetMapper.addYieldValuePusher(yieldPusher.get)
      resultSetMapper.isActive = true
      _isActive = true
    }

  override def toString =
    'ValueSelectElement + ":" + expression.writeToString  
}

/**
 * All nodes that refer to a SelectElement are SelectElementReference,
 * with the exception of SelectElement that refer to an inner or outer query's SelectElement,
 * these are ExportedSelectElement
 */
class SelectElementReference[A,T]
  (val selectElement: SelectElement, val mapper: OutMapper[A])
    extends TypedExpression[A,T] {
    
  override def toString =
    'SelectElementReference + ":" + Utils.failSafeString(delegateAtUseSite.alias) + ":" + selectElement.typeOfExpressionToString + inhibitedFlagForAstDump

  override def inhibited =
    selectElement.inhibited

  private def _useSite: QueryExpressionNode[_] = {
    
    def findQueryExpressionNode(e: ExpressionNode): QueryExpressionNode[_] = e match {
      case qe: QueryExpressionNode[_] => qe
      case _ =>
        e.parent match {
          case Some(e_) => findQueryExpressionNode(e_)
          case _ => 
            org.squeryl.internals.Utils.throwError("could not determine use site of "+ this)
        }
    }
    
    findQueryExpressionNode(this)
  }

  lazy val delegateAtUseSite =
    if(selectElement.parent == None)
      selectElement
    else {
      val us = this._useSite
      if(selectElement.parentQueryable == us)
        selectElement
      else {
        val ese = new ExportedSelectElement(this.selectElement)
        ese.parent = Some(us)
        ese
      }
    }

  override def doWrite(sw: StatementWriter) =
    sw.write(sw.quoteName(delegateAtUseSite.alias))
}

/**
 * SelectElement that refer to a SelectElement of an inner or outer query
 */
class ExportedSelectElement
  (val selectElement: SelectElement)
    extends SelectElement {

  def resultSetMapper = selectElement.resultSetMapper

  override def inhibited =
    selectElement.inhibited

  override def prepareMapper(jdbcIndex: Int) =
    selectElement.prepareMapper(jdbcIndex)

  def prepareColumnMapper(index: Int) =
    selectElement.prepareColumnMapper(index)

  def typeOfExpressionToString =
    selectElement.typeOfExpressionToString

  def origin = selectElement.origin

  val expression = new ExpressionNode {

    def doWrite(sw: StatementWriter) =
    sw.write(sw.quoteName(alias))
  }

  override def toString =
    'ExportedSelectElement + ":" + alias + ",(selectElement=" + selectElement + ")"

  def alias:String =
    if (isDirectOuterReference)
      selectElement.alias
    else
      target.parent.get.asInstanceOf[QueryableExpressionNode].alias + "." + target.aliasSegment

  override def aliasSegment: String =
    //target.parent.get.asInstanceOf[QueryableExpressionNode].alias + "_" + target.aliasSegment
    if (isDirectOuterReference)
      selectElement.aliasSegment
    else
      Session.currentSession.databaseAdapter.aliasExport(
        target.parent.get.asInstanceOf[QueryableExpressionNode], target)

  /**
   * A root level query that has nested queries (or refers to queries in an outer scope) will
   * have SelectElements that are ExportedSelectElement, the 'actualSelectElement' points directly
   * to the refered AST node, while 'target' refers to it indirectly (see target)
   */
  override def actualSelectElement: SelectElement =
    selectElement.actualSelectElement

  /**
   * target points to the selectElement that this ExportSelectElement refers to, who can
   * also be an ExportSelectElement, whose target will point to its inner select element,
   * recursively, until it becomes equal to the 'end' target, the actualSelectElement
   * In other words :
   *   exportSelectElement.target.target.,...,.target == exportSelectElement.actualSelectElement
   */
  lazy val target: SelectElement = innerTarget.getOrElse(
    outerTarget.getOrElse(org.squeryl.internals.Utils.throwError("could not find the target of : " + selectElement))
  )

  private def outerScopes: List[QueryExpressionNode[_]] = outerScopes0(this, Nil)

  @tailrec
  private def outerScopes0(current: ExpressionNode, scopes: List[QueryExpressionNode[_]]): List[QueryExpressionNode[_]] = {
    current.parent match {
      case Some(s: QueryExpressionNode[_]) => outerScopes0(s, scopes :+ s)
      case Some(n) => outerScopes0(n, scopes)
      case None => scopes
    }
  }

  private def isDirectOuterReference: Boolean = outerScopes.exists((outer) => outer == selectElement.parentQueryable)

  private def outerTarget: Option[SelectElement] = {
    val q =
      for (outer <- outerScopes;
           subQuery <- outer.subQueries;
           se <- subQuery.asInstanceOf[QueryExpressionElements].selectList
             if se == selectElement || se.actualSelectElement == selectElement)
      yield se

    q.headOption
  }

  private def innerTarget: Option[SelectElement] =
    if(parent == None)
      return None
    else {
      val parentOfThis = parent.get.asInstanceOf[QueryExpressionElements]

      if(selectElement.origin.parent.get == parentOfThis) {
        Some(selectElement)
      }
      else {
        val q =
          for(q <- parentOfThis.subQueries;
              se <- q.asInstanceOf[QueryExpressionElements].selectList if se == selectElement || se.actualSelectElement == selectElement)
          yield se

        q.headOption
      }
    }
}
