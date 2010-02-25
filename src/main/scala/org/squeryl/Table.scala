package org.squeryl;

import dsl.ast._
import dsl.{QueryDsl}
import internals.{NoOpOutMapper, FieldReferenceLinker, StatementWriter}
import java.sql.{Statement}
import scala.reflect.Manifest


class Table[T] private [squeryl] (n: String, c: Class[T]) extends View[T](n, c) {

  def this(n:String)(implicit manifestT: Manifest[T]) =
    this(n, manifestT.erasure.asInstanceOf[Class[T]])  

  private def _dbAdapter = Session.currentSession.databaseAdapter

  def insert(t: T) = {

    val o = t.asInstanceOf[AnyRef]

    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeInsert(t, this, sw)

    val st = 
      if(_dbAdapter.supportsAutoIncrementInColumnDeclaration)
        Session.currentSession.connection.prepareStatement(sw.statement, Statement.RETURN_GENERATED_KEYS)
      else if( posoMetaData.primaryKey != None) {
        val autoIncPk = new Array[String](1)
        autoIncPk(0) = posoMetaData.primaryKey.get.columnName
        Session.currentSession.connection.prepareStatement(sw.statement, autoIncPk)
      }
      else
        Session.currentSession.connection.prepareStatement(sw.statement)

    val (cnt, s) = _dbAdapter.executeUpdateForInsert(Session.currentSession, sw, st)

    if(cnt != 1)
      error("failed to insert")

    if(posoMetaData.primaryKey != None && posoMetaData.primaryKey.get.isAutoIncremented) {      
      val rs = s.getGeneratedKeys
      assert(rs.next,
        "getGeneratedKeys returned no rows for the auto incremented\n"+
        " primary key of table '" + name + "' JDBC3 feature might not be supported, \n or"+
        " column might not be defined as auto increment")
      posoMetaData.primaryKey.get.setFromResultSet(o, rs, 1)
    }

    t
  }
  
  def update(o: T)(implicit ev: T <:< KeyedEntity[_]) = {

    val dba = Session.currentSession.databaseAdapter
    val sw = new StatementWriter(dba)
    dba.writeUpdate(o, this, sw)

    val (cnt, s) = dba.executeUpdate(Session.currentSession, sw)

    if(cnt != 1)
      error("failed to update")
  }
  
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

  def delete(q: Query[T]): Int = {

    val queryAst = q.ast.asInstanceOf[QueryExpressionElements]
    queryAst.inhibitAliasOnSelectElementReference = true

    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeDelete(this, queryAst.whereClause, sw)

    val (cnt, s) = _dbAdapter.executeUpdate(Session.currentSession, sw)

    cnt
  }

  def deleteWere(whereClause: T => LogicalBoolean)(implicit dsl: QueryDsl): Int =
    delete(dsl.from(this)(t => dsl.where(whereClause(t)).select(t)))      

  private def _takeLastAccessedUntypedFieldReference: SelectElementReference[_] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case Some(n:SelectElement) => new SelectElementReference(n)(NoOpOutMapper)
      case None => error("Thread local does not have a last accessed field... this is a severe bug !")
  }

//  private def _createWhereIdEqualsClause[K](k:K, a: KeyedEntity[K], dsl: QueryDsl) = {
//    a.id
//    val keyFieldNode = _takeLastAccessedUntypedFieldReference
//    val c = new ConstantExpressionNode[K](k, k != null && k.isInstanceOf[String])
//    val wc = new dsl.BinaryOperatorNodeLogicalBoolean(keyFieldNode, c, "=")
//    wc
//  }

  def lookup[K](k: K)(implicit ev: T <:< KeyedEntity[K], dsl: QueryDsl): Option[T] = {

    //TODO: find out why scalac won't let dsl be passed to another method   
    import dsl._
    
    //val q = From(this)(a => where (_createWhereIdEqualsClause(k, a, dsl)) Select(a))
    val q = from(this)(a => dsl.where {
      a.id
      val keyFieldNode = _takeLastAccessedUntypedFieldReference
      val c = new ConstantExpressionNode[K](k, k != null && k.isInstanceOf[String])
      val wc = new BinaryOperatorNodeLogicalBoolean(keyFieldNode, c, "=")
      wc
    } select(a))
    q.headOption

  }

  def delete[K](k: K)(implicit ev: T <:< KeyedEntity[K], dsl: QueryDsl): Boolean  = {

    import dsl._
    //val q = From(this)(a => where (_createWhereIdEqualsClause(k, a, dsl)) Select(a))
    val q = from(this)(a => dsl.where {
      a.id
      val keyFieldNode = _takeLastAccessedUntypedFieldReference
      val c = new ConstantExpressionNode[K](k, k != null && k.isInstanceOf[String])
      val wc = new BinaryOperatorNodeLogicalBoolean(keyFieldNode, c, "=")
      wc
    } select(a))

    val deleteCount = this.delete(q)
    assert(deleteCount <= 1, "Query :\n" + q.dumpAst + "\nshould have deleted at most 1 row but has deleted " + deleteCount)
    deleteCount == 1
  }
}
