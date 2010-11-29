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

  def origin: QueryableExpressionNode

  def resultSetMapper: ResultSetMapper

  def alias: String

  def aliasSuffix: String

  def prepareColumnMapper(index: Int): Unit

  def prepareMapper(jdbcIndex: Int): Unit

  override def inhibited =
    origin.inhibited

  def isActive = _isActive

  var _isActive = false
  
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
 (val origin: QueryableExpressionNode, val expression: ExpressionNode, indexInTuple: Int, isGroupTuple: Boolean)
    extends SelectElement {

  def resultSetMapper: ResultSetMapper = error("refactor me")
  
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
(val origin: QueryableExpressionNode, val fieldMataData: FieldMetaData, val resultSetMapper: ResultSetMapper)
  extends SelectElement {

  def alias = origin.alias + "_" + fieldMataData.columnName

  def aliasSuffix = fieldMataData.columnName
  
  val expression = new ExpressionNode {
    
    def doWrite(sw: StatementWriter) =
      sw.write(origin.alias + "." + fieldMataData.columnName)
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
  (val expression: ExpressionNode, val resultSetMapper: ResultSetMapper, mapper: OutMapper[_], val origin: QueryableExpressionNode)
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


trait PathReferenceToSelectElement {
  self: ExpressionNode =>

  def selectElement: SelectElement

  def doWrite(sw: StatementWriter) = {

    if(_useSite == selectElement.origin.parent.get)
      selectElement.expression.write(sw)
    else
      sw.write(path)
  }
  
  private def _useSite: QueryExpressionNode[_] = {

    var e: ExpressionNode = this

    do {
      e = e.parent.get
      if(e.isInstanceOf[QueryExpressionNode[_]])
        return e.asInstanceOf[QueryExpressionNode[_]]
    } while (e != None)

    error("could not determine use site of "+ this)
  }

  protected def path: String = {

    val origin = selectElement.origin

    if(origin.parent == None)
      return selectElement.alias

    if(origin.parent.get.isInstanceOf[UpdateStatement] ||
       origin.parent.get.asInstanceOf[QueryExpressionElements].inhibitAliasOnSelectElementReference)
      return selectElement.asInstanceOf[FieldSelectElement].fieldMataData.columnName

    val us = _useSite

    val ab = new ArrayBuffer[QueryableExpressionNode]

    var o:ExpressionNode = origin

    do {
      if(o.isInstanceOf[QueryableExpressionNode])
        ab.prepend(o.asInstanceOf[QueryableExpressionNode])
      o = o.parent.get
    } while(o != us && o.parent != None)

    if(ab.size == 1)
      ab.remove(0).alias + "." + selectElement.aliasSuffix
    else
      ab.remove(0).alias + "." + ab.map(n=>n.alias).mkString("_") + "_" + selectElement.aliasSuffix
  }
}


/**
 * All nodes that refer to a SelectElement are SelectElementReference,
 * with the exception of SelectElement that refer to an inner query's SelectElement,
 * these are ExportedSelectElement
 */
class SelectElementReference[A]
  (val selectElement: SelectElement)(implicit val mapper: OutMapper[A])
    extends TypedExpressionNode[A] with PathReferenceToSelectElement {

  override def toString =
    'SelectElementReference + ":" + Utils.failSafeString(path) + ":" + selectElement.typeOfExpressionToString + inhibitedFlagForAstDump

  override def inhibited =
    selectElement.inhibited

  override def doWrite(sw: StatementWriter) =
    sw.write(path)
}

/**
 * SelectElement that refer to a SelectElement of an inner query 
 */
class ExportedSelectElement
  (val selectElement: SelectElement)
    extends SelectElement
    with PathReferenceToSelectElement {

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

    def doWrite(sw: StatementWriter) = error("refactor me")
  }

  def alias = error("refactor me")

  override def toString =
    'ExportedSelectElement + ":" + path

  override def doWrite(sw: StatementWriter) = {
    val p = path
    sw.write(p)
    sw.write(" as ")
    sw.write(p.replace('.','_'))
  }
}
