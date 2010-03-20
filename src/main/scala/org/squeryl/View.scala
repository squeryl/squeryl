package org.squeryl

import internals._
import java.sql.ResultSet

/**
 * This class can be used for read only tables or (database) views
 * for an updatable view, or table use Table[T] 
 */
class View[T] private [squeryl](_name: String, private[squeryl] val classOfT: Class[T], schema: Schema) extends Queryable[T] {

  def this(n:String)(implicit manifestT: Manifest[T]) =
    this(n, manifestT.erasure.asInstanceOf[Class[T]], DummySchema)

  def name = schema.tableNameFromClassName(_name)

  private [squeryl] def findFieldMetaDataForProperty(name: String) = posoMetaData.findFieldMetaDataForProperty(name)

  private [squeryl] val posoMetaData = new PosoMetaData(classOfT, schema)

  private [squeryl] def allFieldsMetaData: Iterable[FieldMetaData] = posoMetaData.fieldsMetaData

  private val _emptyArray = new Array[Object](0);

  private [squeryl] def give(resultSetMapper: ResultSetMapper, resultSet: ResultSet) : T  = {
    
    val c = posoMetaData.constructor

    val o = c._1.newInstance(c._2 :_*).asInstanceOf[AnyRef];
    
    resultSetMapper.map(o, resultSet);
    o.asInstanceOf[T]
  }  
}