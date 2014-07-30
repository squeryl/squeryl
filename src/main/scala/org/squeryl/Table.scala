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
package org.squeryl;

import dsl.ast._
import dsl.{CompositeKey, QueryDsl}
import internals._
import java.sql.{Statement}
import logging.StackMarker
import scala.reflect.Manifest
import collection.mutable.ArrayBuffer
import javax.swing.UIDefaults.LazyValue

//private [squeryl] object DummySchema extends Schema

class Table[T] private [squeryl] (n: String, c: Class[T], val schema: Schema, _prefix: Option[String], ked: Option[KeyedEntityDef[T,_]]) extends View[T](n, c, schema, _prefix, ked) {

  private def _dbAdapter = Session.currentSession.databaseAdapter

  private def hasRefreshableFields: Boolean =
    _dbAdapter.supportsReturningClause && posoMetaData.hasRefreshableFields

  /**
   * @throws SquerylSQLException When a database error occurs or the insert
   * does not result in 1 row
   */
  def insert(t: T): T = StackMarker.lastSquerylStackFrame {

    val o = _callbacks.beforeInsert(t.asInstanceOf[AnyRef])
    val sess = Session.currentSession
    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeInsert(o.asInstanceOf[T], this, sw)
    _dbAdapter.writeReturningClause(this, sw)

    val st =
      (_dbAdapter.supportsAutoIncrementInColumnDeclaration, posoMetaData.primaryKey) match {
        case (true, a:Any) if ! hasRefreshableFields => sess.connection.prepareStatement(sw.statement, Statement.RETURN_GENERATED_KEYS) // HACK PG driver will append its own RETURNING clause
        case (false, Some(Left(pk:FieldMetaData))) if ! hasRefreshableFields => {
          val autoIncPk = new Array[String](1)
          autoIncPk(0) = pk.columnName
          sess.connection.prepareStatement(sw.statement, autoIncPk)
        }
        case a:Any => sess.connection.prepareStatement(sw.statement)
      }

    try {
      val executeResult = _dbAdapter.executeUpdateForInsert(sess, sw, st)

      if (executeResult) {
        if (hasRefreshableFields) {
          val rs = st.getResultSet
          try {
            if (! rs.next())
              throw SquerylSQLException("Expected data")
            for {
              (fmd, index) <- posoMetaData.refreshableFields.zipWithIndex
            } {
              fmd.setFromResultSet(o, rs, index+1)
            }
          } finally {
            rs.close()
          }
          st.getMoreResults()
        } else {
          throw SquerylSQLException("Unexpected result set")
        }
      }

      val cnt = st.getUpdateCount

      //if (cnt != 1)
      if (cnt != 1 && ! hasRefreshableFields) // HACK Work around PG JDBC bug
        throw SquerylSQLException("failed to insert.  Expected 1 row, got " + cnt)

      posoMetaData.primaryKey match {
        case Some(Left(pk:FieldMetaData)) => if(pk.isAutoIncremented) {
          val rs = st.getGeneratedKeys
          try {
            assert(rs.next,
              "getGeneratedKeys returned no rows for the auto incremented\n"+
              " primary key of table '" + name + "' JDBC3 feature might not be supported, \n or"+
              " column might not be defined as auto increment")
            pk.setFromResultSet(o, rs, 1)
          }
          finally {
            rs.close
          }
        }
        case _ =>
      }
    }
    finally {
      st.close
    }

    val r = _callbacks.afterInsert(o).asInstanceOf[T]

    _setPersisted(r)

    r
  }

//  def insert(t: Query[T]) = org.squeryl.internals.Utils.throwError("not implemented")

  def insert[K](e: Iterable[T])(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): Iterable[T] =
    _batchedUpdateOrInsert[K](e, t => posoMetaData.fieldsMetaData.filter(fmd => !fmd.isAutoIncremented && fmd.isInsertable), true, false)

  /**
   * isInsert if statement is insert otherwise update
   */
  private def _batchedUpdateOrInsert[K](e: Iterable[T], fmdCallback: T => Iterable[FieldMetaData], isInsert: Boolean, checkOCC: Boolean)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): Iterable[T] = {

    val it = e.iterator

    if(it.hasNext) {

      val e0 = it.next
      val fmds = fmdCallback(e0)
      val sess = Session.currentSession
      val dba = _dbAdapter
      val sw = new StatementWriter(dba)
      val forAfterUpdateOrInsert = new ArrayBuffer[AnyRef]

      if(isInsert) {
        val z = _callbacks.beforeInsert(e0.asInstanceOf[AnyRef])
        forAfterUpdateOrInsert.append(z)
        dba.writeInsert(z.asInstanceOf[T], this, sw)
      }
      else {
        val z = _callbacks.beforeUpdate(e0.asInstanceOf[AnyRef])
        forAfterUpdateOrInsert.append(z)
        dba.writeUpdate(z.asInstanceOf[T], this, sw, checkOCC)
      }

      if(sess.isLoggingEnabled)
        sess.log("Performing batched " + (if (isInsert) "insert" else "update") + " with " + sw.statement)

      val st = sess.connection.prepareStatement(sw.statement)

      try {
        dba.fillParamsInto(sw.params, st)
        st.addBatch

        var updateCount = 1

        while(it.hasNext) {
          val eN0 = it.next.asInstanceOf[AnyRef]
          val eN =
            if(isInsert)
              _callbacks.beforeInsert(eN0)
            else
              _callbacks.beforeUpdate(eN0)

          forAfterUpdateOrInsert.append(eN)

          var idx = 1
          fmds.foreach(fmd => {
            dba.setParamInto(st, FieldStatementParam(eN, fmd), idx)
            idx += 1
          })
          st.addBatch
          updateCount += 1
        }

        val execResults = st.executeBatch

        if(checkOCC)
          for(b <- execResults)
            if(b == 0) {
              val updateOrInsert = if(isInsert) "insert" else "update"
              throw new StaleUpdateException(
                "Attemped to "+updateOrInsert+" stale object under optimistic concurrency control")
            }
      }
      finally {
        st.close
      }

      for (a <- forAfterUpdateOrInsert) yield {
        val a0 =
          if (posoMetaData.hasRefreshableFields) {
            val r = lookup[K](ked.getId(a.asInstanceOf[T])) getOrElse (throw SquerylSQLException("Could not find record to refresh"))
            r.asInstanceOf[AnyRef]
          } else
            a
        if(isInsert) {
          val c =_callbacks.afterInsert(a0).asInstanceOf[T]
          _setPersisted(c)
          c
        } else {
          _callbacks.afterUpdate(a0).asInstanceOf[T]
        }
      }
    } else Nil
  }

  /**
   * Updates without any Optimistic Concurrency Control check
   * @throws SquerylSQLException When a database error occurs or the update
   * does not result in 1 row
   */
  def forceUpdate[K](o: T)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup) =
    _update(o, false)

  /**
   * @throws SquerylSQLException When a database error occurs or the update
   * does not result in 1 row
   */
  def update[K](o: T)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): T =
    _update[K](o, true)

  def updateModified[K](prev: T, o: T)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): T = {
    copyOccData(prev, o)
    _update(o, true, fmd => fmd.get(prev.asInstanceOf[AnyRef]) != fmd.get(o.asInstanceOf[AnyRef]))
  }

  private def copyOccData(prev: T, o: T) {
    posoMetaData.fieldsMetaData
      .filter (fmd => fmd.isOptimisticCounter || fmd.isPgOptimisticValue)
      .foreach(fmd => fmd.set(o.asInstanceOf[AnyRef], fmd.get(prev.asInstanceOf[AnyRef])))
  }

  def update[K](o: Iterable[T])(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): Iterable[T] =
    _update(o, ked.isOptimistic)

  def forceUpdate[K](o: Iterable[T])(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): Iterable[T] =
    _update(o, ked.isOptimistic)

  private def _update[K](o: T, checkOCC: Boolean, fieldFilter: FieldMetaData => Boolean = _ => true)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): T = {

    val dba = Session.currentSession.databaseAdapter
    val sw = new StatementWriter(dba)
    val o0 = _callbacks.beforeUpdate(o.asInstanceOf[AnyRef]).asInstanceOf[T]
    val runUpdate = dba.writeUpdate(o0, this, sw, checkOCC, fieldFilter)

    val result =
      if (runUpdate) {
        val cnt  = dba.executeUpdateAndCloseStatement(Session.currentSession, sw)

        if(cnt != 1) {
          if(checkOCC && posoMetaData.isOptimistic) {
            val version = posoMetaData.optimisticCounter.get.getNativeJdbcValue(o.asInstanceOf[AnyRef])
            throw new StaleUpdateException(
              "Object "+prefixedName + "(id=" + ked.getId(o) + ", occVersionNumber=" + version +
              ") has become stale, it cannot be updated under optimistic concurrency control")
          }
          else if (checkOCC && posoMetaData.isPgOptimistic) {
            throw new StaleUpdateException(
              "Object "+prefixedName + "(id=" + o.asInstanceOf[KeyedEntity[_]].id + ", " +
                posoMetaData.pgOptimisticDescr(o.asInstanceOf[AnyRef]) +
                ") has become stale, it cannot be updated under optimistic concurrency control")
          }
          else
            throw SquerylSQLException("failed to update.  Expected 1 row, got " + cnt)
        }

        // HACK Can't use RETURNING here due to PG JDBC bug (no update counts with RETURNING)
        if (posoMetaData.hasRefreshableFields)
          lookup(ked.getId(o0)) getOrElse (throw SquerylSQLException("Could not find record to refresh"))
        else
          o0
    } else {
      o0
    }

    _callbacks.afterUpdate(result.asInstanceOf[AnyRef]).asInstanceOf[T]
  }

  private def _update[K](e: Iterable[T], checkOCC: Boolean)(implicit ked: KeyedEntityDef[T, K], dsl: QueryDsl, toCanLookup: K => CanLookup): Iterable[T] = {

    def buildFmds(t: T): Iterable[FieldMetaData] = {
      val pkList = posoMetaData.primaryKey.getOrElse(
        org.squeryl.internals.Utils.throwError("method was called with " + posoMetaData.clasz.getName + " that is not a KeyedEntity[]")).fold(
        pkMd => List(pkMd),
        pkGetter => {
          // Just for side-effect...
          var fields: Option[List[FieldMetaData]]  = None
          Utils.createQuery4WhereClause(this, (t0:T) => {
            val ck = pkGetter.invoke(t0).asInstanceOf[CompositeKey]

            fields = Some(ck._fields.toList)

            new EqualityExpression(InternalFieldMapper.intTEF.createConstant(1), InternalFieldMapper.intTEF.createConstant(1))
          })

          fields getOrElse (internals.Utils.throwError("No PK fields found"))
        }
      )

      List(
        posoMetaData.fieldsMetaData.filter(fmd=> ! fmd.isIdFieldOfKeyedEntity && ! fmd.isOptimisticCounter && fmd.isUpdatable).toList,
        pkList,
        posoMetaData.optimisticCounter.toList,
        posoMetaData.pgOptimisticValues.toList
      ).flatten
    }

    _batchedUpdateOrInsert[K](e, buildFmds _, false, checkOCC)
  }

  def update(s: T =>UpdateStatement):Int = {

    val vxn = new ViewExpressionNode(this)
    vxn.sample =
       posoMetaData.createSample(FieldReferenceLinker.createCallBack(vxn))
    val us = s(vxn.sample)
    vxn.parent = Some(us)

    var idGen = 0
    us.visitDescendants((node,parent,i) => {

      if(node.parent == None)
        node.parent = parent

      if(node.isInstanceOf[UniqueIdInAliaseRequired]) {
        val nxn = node.asInstanceOf[UniqueIdInAliaseRequired]
        nxn.uniqueId = Some(idGen)
        idGen += 1
      }
    })

    vxn.uniqueId = Some(idGen)

    val dba = _dbAdapter
    val sw = new StatementWriter(dba)
    dba.writeUpdate(this, us, sw)
    dba.executeUpdateAndCloseStatement(Session.currentSession, sw)
  }

  def delete(q: Query[T]): Int = {

    val queryAst = q.ast.asInstanceOf[QueryExpressionElements]
    queryAst.inhibitAliasOnSelectElementReference = true

    val sw = new StatementWriter(_dbAdapter)
    _dbAdapter.writeDelete(this, queryAst.whereClause, sw)

    _dbAdapter.executeUpdateAndCloseStatement(Session.currentSession, sw)
  }

  def deleteWhere(whereClause: T => LogicalBoolean)(implicit dsl: QueryDsl): Int =
    delete(dsl.from(this)(t => dsl.where(whereClause(t)).select(t)))

  def delete[K](k: K)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): Boolean  = {
    import dsl._
    val q = from(this)(a => dsl.where {
      FieldReferenceLinker.createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(ked.getId(a), k, toCanLookup(k))
    } select(a))

    lazy val z = q.headOption

    if(_callbacks.hasBeforeDelete) {
      z.map(x => _callbacks.beforeDelete(x.asInstanceOf[AnyRef]))
    }

    val deleteCount = this.delete(q)

    if(_callbacks.hasAfterDelete) {
      z.map(x => _callbacks.afterDelete(x.asInstanceOf[AnyRef]))
    }

    if(Session.currentSessionOption map { ses => ses.databaseAdapter.verifyDeleteByPK } getOrElse true)
      assert(deleteCount <= 1, "Query :\n" + q.dumpAst + "\nshould have deleted at most 1 row but has deleted " + deleteCount)
    deleteCount > 0
  }

  /**
   * @throws SquerylSQLException When a database error occurs or the operation
   * does not result in 1 row
   */
  def insertOrUpdate[K](o: T)(implicit ked: KeyedEntityDef[T,K], dsl: QueryDsl, toCanLookup: K => CanLookup): T = {
    if(ked.isPersisted(o))
      update(o)
    else
      insert(o)
    o
  }

  def assocInsertOrUpdate(o: T)(implicit ked: KeyedEntityDef[T,_], dsl: QueryDsl): T = {
    if(ked.isPersisted(o))
      update[Nothing](o)(ked.asInstanceOf[KeyedEntityDef[T,Nothing]], dsl, _ => UnknownCanLookup)
    else
      insert(o)
    o
  }
}
