package org.squeryl.internals

import java.sql.ResultSet
import collection.mutable.ArrayBuffer
import org.squeryl.dsl.ast.SelectElement


trait OutMapper[T] {

  override def toString =
    "$OM(" + index + "," + sample.asInstanceOf[AnyRef].getClass.getSimpleName + ")"

  var index: Int = -1

  var isActive = false
  
  def map(rs: ResultSet): T =
    if(isActive)
      doMap(rs)
    else
      sample

  def isNull(rs: ResultSet) =
    rs.getObject(index) == null

  def doMap(rs: ResultSet): T

  def sample: T

  def typeOfExpressionToString = sample.asInstanceOf[Object].getClass.getName
}


class ColumnToFieldMapper(val index: Int, val fieldMetaData: FieldMetaData, selectElement: SelectElement)  {

  if(index <= 0)
    error("invalid Jdbc index " + index)

  def map(obj: AnyRef, rs: ResultSet): Unit = {

    if(selectElement.isActive) {
      fieldMetaData.setFromResultSet(obj, rs, index)
    }
  }

  override def toString =
    "$(" + index + "->" + fieldMetaData + ")"
}

class ColumnToTupleMapper(val outMappers: Array[OutMapper[_]]) {

  override def toString = outMappers.mkString("(",",",")") 

  def typeOfExpressionToString(idx: Int) = outMappers.apply(idx).typeOfExpressionToString

  def activate(i: Int) = outMappers.apply(i).isActive = true

  def isActive(i: Int) = outMappers.apply(i).isActive

  def isNull(i:Int, rs: ResultSet) = outMappers.apply(i).isNull(rs)
  
  def mapToTuple[T](rs: ResultSet): T = {
    val size = outMappers.size
    val m = outMappers
    val res = size match {
      case 1 => (m(0).map(rs))
      case 2 => (m(0).map(rs), m(1).map(rs))
      case 3 => (m(0).map(rs), m(1).map(rs), m(2).map(rs))
      case 4 => (m(0).map(rs), m(1).map(rs), m(2).map(rs), m(3).map(rs))
      case 5 => (m(0).map(rs), m(1).map(rs), m(2).map(rs), m(3).map(rs), m(4).map(rs))
      case 6 => (m(0).map(rs), m(1).map(rs), m(2).map(rs), m(3).map(rs), m(4).map(rs), m(5).map(rs))
      case 7 => (m(0).map(rs), m(1).map(rs), m(2).map(rs), m(3).map(rs), m(4).map(rs), m(5).map(rs), m(6).map(rs))
      //TODO: implement tuples results of size up to 22
      case z:Any => error("tuples of size "+size+" and greater are not supported")
    }
    
    res.asInstanceOf[T]
  }
}

class ResultSetMapper {  

  private val _yieldValuePushers = new ArrayBuffer[YieldValuePusher]

  private val _fieldMapper = new ArrayBuffer[ColumnToFieldMapper]

  var groupKeysMapper: Option[ColumnToTupleMapper] = None

  var groupMeasuresMapper: Option[ColumnToTupleMapper] = None
  
//  val createTrace = {
//    new Exception().getStackTrace.map(s=>s.toString).mkString("\n")
//  }

  var isActive = false

  override def toString =
    'ResultSetMapper + ":" + Integer.toHexString(System.identityHashCode(this)) +
     _fieldMapper.mkString("(",",",")") +
    "-" + groupKeysMapper.getOrElse("") +
    "-" + groupMeasuresMapper.getOrElse("")


  def addColumnMapper(cm: ColumnToFieldMapper) =
    _fieldMapper.append(cm)

  def addYieldValuePusher(yvp: YieldValuePusher) =
    _yieldValuePushers.append(yvp)

  def pushYieldedValues(resultSet: ResultSet):Unit = {

    if(!isActive)
      return

    for(yvp <- _yieldValuePushers)
      yvp.push(resultSet)
  }

  def isNoneInOuterJoin(rs: ResultSet): Boolean = {
    for(c2fm <- _fieldMapper)
      if(!c2fm.fieldMetaData.isOption)
        return rs.getObject(c2fm.index) == null
    false
  }

  def map(o: AnyRef, resultSet: ResultSet):Unit = {

    if(!isActive)
      return

    try {
      for(fm <- _fieldMapper)
        fm.map(o, resultSet)      
    }
    catch {
      case e:Exception=> {
        throw new RuntimeException("could not map row :\n" + dumpRow(resultSet) + "\n with mapper :\n" + this, e)
      }
    }
  }

  def dumpRow(rs:ResultSet) = {
    val md = rs.getMetaData
   (for(i <- 1 to md.getColumnCount)
      yield ""+rs.getObject(i)+":"+md.getColumnClassName(i)).mkString("[",",","]")
  }

  def dumpRowValues(rs:ResultSet) = {
    val md = rs.getMetaData
   (for(i <- 1 to md.getColumnCount)
      yield ""+rs.getObject(i)).mkString("[",",","]")
  }
}

class YieldValuePusher(val index: Int, val selectElement: SelectElement, expressionType: Class[_])  {

  val resultSetHandler =
    FieldMetaData.resultSetHandlerFor(expressionType)

  def push(rs: ResultSet) = {

    if(selectElement.isActive) {
      val v = resultSetHandler(rs,index)
      FieldReferenceLinker.pushYieldValue(v)
    }
  }

  override def toString =
    "$(" + index + "->Value("+selectElement.writeToString+")"
}