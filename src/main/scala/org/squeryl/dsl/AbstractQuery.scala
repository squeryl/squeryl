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
package org.squeryl.dsl

import ast.{QueryableExpressionNode, ViewExpressionNode, ExpressionNode, QueryExpressionNode}
import internal.{InnerJoinedQueryable, OuterJoinedQueryable}
import java.sql.ResultSet
import org.squeryl.internals._
import org.squeryl.{View, Queryable, Session, Query}
import collection.mutable.ArrayBuffer

abstract class AbstractQuery[R](val isRoot:Boolean) extends Query[R] {

  var selectDistinct = false
  
  var isForUpdate = false

  var page: Option[(Int,Int)] = None


  val resultSetMapper = new ResultSetMapper

  val name = "query"

  def give(rsm: ResultSetMapper, rs: ResultSet): R = {
    rsm.pushYieldedValues(rs)
    val r = invokeYield(rsm, rs)
    r
  }

  protected def buildAst(qy: QueryYield[R], subQueryables: SubQueryable[_]*) = {


    val subQueries = new ArrayBuffer[QueryableExpressionNode]

    val views = new ArrayBuffer[ViewExpressionNode[_]]

    if(qy.joinExpressions != Nil) {
      val sqIterator = subQueryables.iterator
      val joinExprsIterator = qy.joinExpressions.iterator 
      sqIterator.next // get rid of the first one

      while(sqIterator.hasNext) {
        val nthQueryable = sqIterator.next
        val nthJoinExpr = joinExprsIterator.next
        nthQueryable.node.joinExpression = Some(nthJoinExpr())
      }
    }

    for(sq <- subQueryables)
      if(! sq.isQuery)
        views.append(sq.node.asInstanceOf[ViewExpressionNode[_]])

    for(sq <- subQueryables)
      if(sq.isQuery)
        subQueries.append(sq.node.asInstanceOf[QueryExpressionNode[_]])
    
    val qen = new QueryExpressionNode[R](this, qy, subQueries, views)
    val (sl,d) = qy.invokeYieldForAst(qen, resultSetMapper)
    qen.setOutExpressionNodesAndSample(sl, d)
    qen
  }

  def ast: QueryExpressionNode[R]

  def copy(asRoot:Boolean) = {
    val c = createCopy(asRoot)
    c.selectDistinct = selectDistinct
    c
  }

  def createCopy(asRoot:Boolean): AbstractQuery[R]

  def dumpAst = ast.dumpAst

  def statement: String = _genStatement(true)

  private def _genStatement(forDisplay: Boolean) = {

    val sw = new StatementWriter(forDisplay, Session.currentSession.databaseAdapter)
    ast.write(sw)
    sw.statement
  }

  def distinct = {
    val c = copy(true)
    c.selectDistinct = true;
    c
  }

  def page(offset: Int, pageLength: Int): Query[R] = {
    val c = copy(true)
    c.page = Some((offset, pageLength))
    c    
  }

  def forUpdate = {
    val c = copy(true)
    c.isForUpdate = true;
    c    
  }

  private def _dbAdapter = Session.currentSession.databaseAdapter

  override def iterator = new Iterator[R] {

    val sw = new StatementWriter(false, _dbAdapter)
    ast.write(sw)
    val s = Session.currentSession
    val (rs, stmt) = _dbAdapter.executeQuery(s, sw)
    s._addStatement(stmt) // if the iteration doesn't get completed, we must hang on to the statement to clean it up at session end.
    s._addResultSet(rs) // same for the result set
    
    var _nextCalled = false;
    var _hasNext = false;

    def _next = {
      _hasNext = rs.next

      if(!_hasNext) {// close it since we've completed the iteration
        Utils.close(rs)
        stmt.close
      }

      _nextCalled = true
    }

    def hasNext = {
      if(!_nextCalled)
        _next
      _hasNext
    }

    def next: R = {
      if(!_nextCalled)
        _next
      if(!_hasNext)
        error("next called with no rows available")
      _nextCalled = false

      if(s.isLoggingEnabled)
        s.log(ResultSetUtils.dumpRow(rs))

      give(resultSetMapper, rs)
    }
  }

  override def toString = dumpAst + "\n" + _genStatement(true)

  protected def createSubQueryable[U](q: Queryable[U]): SubQueryable[U] =
    if(q.isInstanceOf[View[_]]) {
      val v = q.asInstanceOf[View[U]]
      val vxn = new ViewExpressionNode(v)
      vxn.sample =
        v.posoMetaData.createSample(FieldReferenceLinker.createCallBack(vxn))
      
      new SubQueryable(v, vxn.sample, vxn.resultSetMapper, false, vxn)
    }
    else if(q.isInstanceOf[OptionalQueryable[U]]) {
      val oqr = q.asInstanceOf[OptionalQueryable[U]]
      val sq = createSubQueryable[U](oqr.queryable)
      sq.node.inhibited = oqr.inhibited
      new SubQueryable(q, Some(sq.sample).asInstanceOf[U], sq.resultSetMapper, sq.isQuery, sq.node)
    }
    else if(q.isInstanceOf[OuterJoinedQueryable[U]]) {
      val ojq = q.asInstanceOf[OuterJoinedQueryable[U]]
      val sq = createSubQueryable[U](ojq.queryable)
      sq.node.joinKind = Some((ojq.leftRightOrFull, "outer"))
      new SubQueryable(ojq.queryable, Some(sq.sample).asInstanceOf[U], sq.resultSetMapper, sq.isQuery, sq.node)
    }
    else if(q.isInstanceOf[InnerJoinedQueryable[U]]) {
      val ijq = q.asInstanceOf[InnerJoinedQueryable[U]]
      val sq = createSubQueryable[U](ijq.queryable)
      sq.node.joinKind = Some((ijq.leftRightOrFull, "inner"))
      new SubQueryable(ijq.queryable, sq.sample, sq.resultSetMapper, sq.isQuery, sq.node)
    }
    else if(q.isInstanceOf[DelegateQuery[U]]) {
      createSubQueryable(q.asInstanceOf[DelegateQuery[U]].q)
    }      
    else {
      val qr = q.asInstanceOf[AbstractQuery[U]]
      val copy = qr.copy(false)
      new SubQueryable(copy, copy.ast.sample.asInstanceOf[U], copy.resultSetMapper, true, copy.ast)
    }

  protected class SubQueryable[U]
    (val queryable: Queryable[U],
     val sample: U,
     val resultSetMapper: ResultSetMapper,
     val isQuery:Boolean,
     val node: QueryableExpressionNode) {

    def give(rs: ResultSet): U =
      if(node.joinKind != None) {
        if(node.isOuterJoined) {
           if(resultSetMapper.isNoneInOuterJoin(rs))
             None.asInstanceOf[U]
           else
             Some(queryable.give(resultSetMapper, rs)).asInstanceOf[U]
        }
        else
          queryable.give(resultSetMapper, rs)
      }
      else if((node.isRightJoined || node.isOuterJoinedDEPRECATED) && resultSetMapper.isNoneInOuterJoin(rs))
        sample
      else
        queryable.give(resultSetMapper, rs)
  }
}
