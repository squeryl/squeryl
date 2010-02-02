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
    if(e == None) {
      val r = new FieldSelectElement(this, fmd, resultSetMapper)
      _selectElements.put(fmd, r)
      r
    }
    else {
      if(export)
        new ExportedSelectElement(e.get)
      else
        e.get
    }
  }

  def getOrCreateSelectElement(fmd: FieldMetaData): SelectElement =
    getOrCreateSelectElement(fmd: FieldMetaData, false)

  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements): SelectElement =
    getOrCreateSelectElement(fmd: FieldMetaData, ! forScope.isChild(this))


  val resultSetMapper = new ResultSetMapper

  def alias = view.name + uniqueId.get

  def owns(aSample: AnyRef) = aSample eq sample.asInstanceOf[AnyRef]

  private var _sample: Option[U] = None

  private[squeryl] def sample_=(d:U) =
    _sample = Some(d)

  def sample = _sample.get

  def doWrite(sw: StatementWriter) =
    sw.write(view.name)

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
