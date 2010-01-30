package org.squeryl;

import dsl.ast.{UniqueIdInAliaseRequired, ViewExpressionNode, UpdateStatement}
import internals.{FieldReferenceLinker, StatementWriter}
import scala.reflect.Manifest
import java.sql.{Statement, SQLException}


class Table[T] private [squeryl] (n: String, c: Class[T]) extends View[T](n, c) {

  def this(n:String)(implicit manifestT: Manifest[T]) =
    this(n, manifestT.erasure.asInstanceOf[Class[T]])  

  private def _dbAdapter = Session.currentSession.databaseAdapter

  def insert(t: T) = {

    val o = t.asInstanceOf[AnyRef]

    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeInsert(t, this, sw)

    val st = 
      if(_dbAdapter.areAutoIncrementFieldsSupported || posoMetaData.primaryKey == None)
        Session.currentSession.connection.prepareStatement(sw.statement, Statement.RETURN_GENERATED_KEYS)
      else {
        val autoIncPk = new Array[String](1)
        autoIncPk(0) = posoMetaData.primaryKey.get.name
        Session.currentSession.connection.prepareStatement(sw.statement, autoIncPk)
      }

    val (cnt, s) = _dbAdapter.executeUpdateForInsert(Session.currentSession, sw, st)

    if(cnt != 1)
      error("failed to insert")

    val rs = s.getGeneratedKeys

    if(posoMetaData.primaryKey != None && posoMetaData.primaryKey.get.isAutoIncremented) {
      assert(rs.next,
        "getGeneratedKeys returned no rows for the auto incremented\n"+
        " primary key of table '" + name + "' JDBC3 feature might not be supported, \n or"+
        " column might not be defined as auto increment")
      posoMetaData.primaryKey.get.setFromResultSet(o, rs, 1)
    }

    t
  }

  //TODO : def update(o: T) in KeyedTable and leave only update(o: T, T=>whereClause) in this table 
  posoMetaData.primaryKey.getOrElse(error(name + " has no primaryKey, tables without primary keys are not yet supported"))

  def update(o: T) = {

    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeUpdate(o, this, sw)

    val (cnt, s) = _dbAdapter.executeUpdate(Session.currentSession, sw)

    if(cnt != 1)
      error("failed to update")
  }

  //def update[P <: Product](updateColsAndWhereClause: T => (P,TypedExpressionNode[Scalar, LogicalBoolean]))(values: P): Int = {0}

  def update(s: T =>UpdateStatement):Int = {

    val vxn = new ViewExpressionNode(this)
    vxn.sample =
       posoMetaData.createSample(FieldReferenceLinker.createCallBack(vxn))    
    val us = s(vxn.sample)
    vxn.parent = Some(us)

    var idGen = 0
    vxn.visitDescendants((node,parent,i) => {

      if(node.parent == None)
        node.parent = parent

      if(node.isInstanceOf[UniqueIdInAliaseRequired]) {
        val nxn = node.asInstanceOf[UniqueIdInAliaseRequired]
        nxn.uniqueId = Some(idGen)
        idGen += 1
      }
    })

    val dba = _dbAdapter
    val sw = new StatementWriter(dba)
    dba.writeUpdate(this, us, sw)
    val res = dba.executeUpdate(Session.currentSession, sw)
    res._1    
  }
}
