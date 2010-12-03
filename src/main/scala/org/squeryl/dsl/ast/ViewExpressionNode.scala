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

import org.squeryl.dsl._
import collection.mutable.HashMap
import org.squeryl.internals.{StatementWriter, ResultSetMapper, FieldMetaData}
import org.squeryl.{Session, View}

class ViewExpressionNode[U](val view: View[U])
  extends QueryableExpressionNode {

  private val _selectElements = new HashMap[FieldMetaData,SelectElement]

  def isChild(q: QueryableExpressionNode) = false

  def getOrCreateAllSelectElements(forScope: QueryExpressionElements): Iterable[SelectElement] = {

    val export = ! forScope.isChild(this)

    view.posoMetaData.fieldsMetaData.map(fmd =>
      getOrCreateSelectElement(fmd, export)
    )
  }

  private def getOrCreateSelectElement(fmd: FieldMetaData, export: Boolean): SelectElement = {

    val e = _selectElements.get(fmd)
    val n =
      if(e != None)
        e.get
      else {
        val r = new FieldSelectElement(this, fmd, resultSetMapper)
        _selectElements.put(fmd, r)
        r
      }

    if(export)
      new ExportedSelectElement(n)
    else
      n
  }

  def getOrCreateSelectElement(fmd: FieldMetaData): SelectElement =
    getOrCreateSelectElement(fmd, false)

  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements): SelectElement =
    getOrCreateSelectElement(fmd, ! forScope.isChild(this))


  val resultSetMapper = new ResultSetMapper

  def alias =
    if(view.prefix != None)
      view.prefix.get + "_" + view.name + uniqueId.get
    else
      view.name + uniqueId.get

  def owns(aSample: AnyRef) = aSample eq sample.asInstanceOf[AnyRef]

  private var _sample: Option[U] = None

  private[squeryl] def sample_=(d:U) =
    _sample = Some(d)

  def sample = _sample.get

  def doWrite(sw: StatementWriter) =
      sw.write(view.prefixedName)

  override def toString = {
    val sb = new StringBuffer
    sb.append('ViewExpressionNode +"[")
    sb.append(sample)
    sb.append("]:")
    dumpOuterJoinInfoForAst(sb)
    sb.append("rsm=")
    sb.append(resultSetMapper)
    sb.toString
  }
}
