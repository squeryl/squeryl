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
 ******************************************************************************/
package org.squeryl.dsl.ast

import collection.mutable.ArrayBuffer
import org.squeryl.internals._
import java.sql.ResultSet

/**
 * SelectElement are elements of a select list, they are either
 *  ValueSelectElement for composite expressions, i.e. select (x / 2) * y as Z from ....
 *  TupleSelectElement for group by or compute elements (TODO: document group by/compute)
 *  FieldSelectElement for table columns (that map to fields)
 *
 *  ExportSelectElement is a select element that refers to a SelectElement of an inner query.
 *
 * SelectElementReference are nodes in any clause other than select (where, having, composite expression, order by, etc)
 *  that refer to a SelectElement  
 */
trait SelectElement extends ExpressionNode {

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

  def aliasSuffix: String

  def aliasComponent: String =
    alias

  def actualSelectElement: SelectElement = this

  def inhibitAliasOnSelectElementReference: Boolean = {
    var e:ExpressionNode = origin

    while(e.parent != None) {
      e = e.parent.get
    }

    if(!e.isInstanceOf[QueryExpressionElements])
      true
    else
      e.asInstanceOf[QueryExpressionElements].inhibitAliasOnSelectElementReference
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

  /**
   * Will throw a ClassCastException if this type is not a Enumeration#Value
   */
  def createEnumerationMapper: OutMapper[Enumeration#Value] = new OutMapper[Enumeration#Value]() {

    def doMap(rs: ResultSet) = {
      val fmd = this.asInstanceOf[FieldSelectElement].fieldMataData
      fmd.canonicalEnumerationValueFor(rs.getInt(this.index))
    }

    def sample = error("!")
  }

  /**
   * Will throw a ClassCastException if this type is not a Enumeration#Value
   */
  def createEnumerationOptionMapper: OutMapper[Option[Enumeration#Value]] = new OutMapper[Option[Enumeration#Value]]() {

    def doMap(rs: ResultSet) = {
      val fmd = this.asInstanceOf[FieldSelectElement].fieldMataData
      Some(fmd.canonicalEnumerationValueFor(rs.getInt(this.index)))
    }

    def sample = error("!")
  }
}

class TupleSelectElement
 (val origin: QueryExpressionNode[_], val expression: ExpressionNode, indexInTuple: Int, isGroupTuple: Boolean)
    extends SelectElement {

  def resultSetMapper: ResultSetMapper = error("refactor me")

  //TODO: normalize ?
  def alias =
    if(isGroupTuple)
      "g" + indexInTuple
    else
      "c" + indexInTuple

  def aliasSuffix = alias
  
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
(val origin: ViewExpressionNode[_], val fieldMataData: FieldMetaData, val resultSetMapper: ResultSetMapper)
  extends SelectElement {

  def alias =
    if(inhibitAliasOnSelectElementReference)
      fieldMataData.columnName
    else
      origin.alias + "." + fieldMataData.columnName

  def aliasSuffix = fieldMataData.columnName

  override def aliasComponent: String =
    origin.alias + "_" + fieldMataData.columnName
  
  val expression = new ExpressionNode {
    
    def doWrite(sw: StatementWriter) =
      sw.write(alias)
  }

  def prepareColumnMapper(index: Int) =
    columnMapper = Some(new ColumnToFieldMapper(index, fieldMataData, this))

  private var columnMapper: Option[ColumnToFieldMapper] = None

  def prepareMapper(jdbcIndex: Int) =
    if(columnMapper != None) {
      resultSetMapper.addColumnMapper(columnMapper.get)
      resultSetMapper.isActive = true
      _isActive = true
    }
  
  def typeOfExpressionToString =
    fieldMataData.displayType
  
  override def toString =
    'FieldSelectElement + ":" +
       Utils.failSafeString(alias, fieldMataData.nameOfProperty)
}

class ValueSelectElement
  (val expression: ExpressionNode, val resultSetMapper: ResultSetMapper, mapper: OutMapper[_], val origin: QueryExpressionNode[_])
     extends SelectElement with UniqueIdInAliaseRequired {

  def alias = "v" + uniqueId.get

  def aliasSuffix = alias

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
 * with the exception of SelectElement that refer to an inner query's SelectElement,
 * these are ExportedSelectElement
 */
class SelectElementReference[A]
  (val selectElement: SelectElement)(implicit val mapper: OutMapper[A])
    extends TypedExpressionNode[A]  {

  override def toString =
    'SelectElementReference + ":" + Utils.failSafeString(_delegateAtUseSite.alias) + ":" + selectElement.typeOfExpressionToString + inhibitedFlagForAstDump

  override def inhibited =
    selectElement.inhibited

  private def _useSite: QueryExpressionNode[_] = {

    var e: ExpressionNode = this

    do {
      e = e.parent.get
      if(e.isInstanceOf[QueryExpressionNode[_]])
        return e.asInstanceOf[QueryExpressionNode[_]]
    } while (e != None)

    error("could not determine use site of "+ this)
  }

  private lazy val _delegateAtUseSite =
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
    sw.write(_delegateAtUseSite.alias)
}

/**
 * SelectElement that refer to a SelectElement of an inner query 
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

  def aliasSuffix = selectElement.aliasSuffix

  val expression = new ExpressionNode {

    def doWrite(sw: StatementWriter) =
      sw.write(alias)
  }

  override def toString =
    'ExportedSelectElement + ":" + alias + ",(selectElement=" + selectElement + ")"

  def alias:String =
    target.parent.get.asInstanceOf[QueryableExpressionNode].alias + "." + target.aliasComponent

  override def aliasComponent: String = {
    target.parent.get.asInstanceOf[QueryableExpressionNode].alias + "_" + target.aliasComponent
  }

  override def actualSelectElement: SelectElement = {
    if(selectElement.isInstanceOf[ExportedSelectElement])
      selectElement.asInstanceOf[ExportedSelectElement].actualSelectElement
    else
      selectElement
  }

  lazy val target: SelectElement = {

    val parentOfThis = parent.get.asInstanceOf[QueryExpressionElements]

    if(selectElement.origin.parent.get == parentOfThis) {
      selectElement
    }
    else {

      val q =
        for(q <- parentOfThis.subQueries;
            se <- q.asInstanceOf[QueryExpressionElements].selectList if se == selectElement || se.actualSelectElement == selectElement)
        yield se

      val r = q.headOption.getOrElse(error("!!!!!!!!!!!!!" + selectElement))
      r
    }
  }  
}
