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
package org.squeryl.internals

import java.sql.ResultSet
import collection.mutable.ArrayBuffer
import org.squeryl.dsl.ast.SelectElement



trait ResultSetUtils {

  def dumpRow(rs:ResultSet) = {
    val md = rs.getMetaData
   (for(i <- 1 to md.getColumnCount)
      yield ""+rs.getObject(i)+":"+_simpleClassName(md.getColumnClassName(i)))
    .mkString("ResultSetRow:[",",","]")
  }

  private def _simpleClassName(className: String) = {
    val idx = className.lastIndexOf(".")
    if(idx < 0)
      className
    else
      className.substring(idx + 1, className.length)
  }

  def dumpRowValues(rs:ResultSet) = {
    val md = rs.getMetaData
   (for(i <- 1 to md.getColumnCount)
      yield ""+rs.getObject(i)).mkString("[",",","]")
  }  
}

object ResultSetUtils extends ResultSetUtils


trait OutMapper[T] extends ResultSetUtils {

  override def toString =
    "$OM(" + index + "," +
    sample.asInstanceOf[AnyRef].getClass.getSimpleName + ")" +
    (if(isActive) "*" else "")

  var index: Int = -1

  var isActive = false
  
  def map(rs: ResultSet): T =
    if(isActive)
      try {
        doMap(rs)
      }
      catch {
        case e:Exception => throw new RuntimeException(
          "Exception while mapping column with OutMapper:\n" + this + "\nand resultSet :\n" + dumpRow(rs))
      }
    else
      sample

  def isNull(rs: ResultSet) =
    rs.getObject(index) == null

  def doMap(rs: ResultSet): T

  def sample: T

  def typeOfExpressionToString = sample.asInstanceOf[Object].getClass.getName

}

object NoOpOutMapper extends OutMapper[Any] {

  def doMap(rs: ResultSet) = sample

  def sample = error(" cannot use NoOpOutMapper")

  override def typeOfExpressionToString = "NoOpOutMapper"  
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

  def activate(i: Int, jdbcIndex: Int) = {
    val m = outMappers.apply(i)
    m.isActive = true
    m.index = jdbcIndex
  }

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

class ResultSetMapper extends ResultSetUtils {  

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
    "-" + groupMeasuresMapper.getOrElse("") +
    (if(isActive) "*" else "")


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

  private lazy val _firstNonOption: Option[ColumnToFieldMapper] =
    _fieldMapper.find(!_.fieldMetaData.isOption)

  def isNoneInOuterJoin(rs: ResultSet): Boolean = {

    //decide based on the nullity of the first non Option field :

    if(_firstNonOption != None) {
      return rs.getObject(_firstNonOption.get.index) == null
    }

    //if we get here all fields are optional, decide on the first Option that is not null :    
    for(c2fm <- _fieldMapper) {
      assert(c2fm.fieldMetaData.isOption)
      if(rs.getObject(c2fm.index) != null)
        return false
    }
    // if we get here we have matched on a row wit all nulls OR we haven't matched,
    // this is an extreme corner case, we will return None, in reality we should
    // sometimes return a Some with all fields None
    true
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
}

class YieldValuePusher(val index: Int, val selectElement: SelectElement, mapper: OutMapper[_])  {


  mapper.index = index
  mapper.isActive = true

  def push(rs: ResultSet) = {

    if(selectElement.isActive) {
      val v = mapper.map(rs)
      FieldReferenceLinker.pushYieldValue(v.asInstanceOf[AnyRef])
    }
  }


  override def toString =
    "$(" + index + "->&("+selectElement.writeToString+")" +
    (if(mapper.isActive) "*" else "")
}
