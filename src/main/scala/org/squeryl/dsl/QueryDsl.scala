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
 ******************************************************************************/
package org.squeryl.dsl

import ast._
import boilerplate._
import fsm.{QueryElements, StartState, WhereState}
import org.squeryl.internals._
import org.squeryl._
import java.sql.{SQLException, ResultSet}

trait QueryDsl
  extends DslFactory
  with WhereState
  with ComputeMeasuresSignaturesFromStartOrWhereState
  with StartState
  with QueryElements
  with FromSignatures {

  def using[A](session: Session)(a: =>A): A =
    _using(session, a _)

  private def _using[A](session: Session, a: ()=>A): A =
    try {
      session.bindToCurrentThread
      val r = a()
      session.unbindFromCurrentThread
      r
    }
    finally {
      session.cleanup
    }

  /**
   * 'transacton' causes a new transaction to begin and commit after the block exection, or rollback
   * if an exception occurs. Invoking a transaction always cause a new one to
   * be created, even if called in the context of an existing transaction.
   */
  def transaction[A](a: =>A): A =
    if(! Session.hasCurrentSession)
      _executeTransactionWithin(SessionFactory.newSession, a _)
    else {
      val s = Session.currentSession
      s.unbindFromCurrentThread
      val res = _executeTransactionWithin(SessionFactory.newSession, a _)
      s.bindToCurrentThread
      res
    }

  /**
   * 'inTransaction' will create a new transaction if none is in progress and commit it upon
   * completion or rollback on exceptions. If a transaction already exists, it has no
   * effect, the block will execute in the context of the existing transaction. The
   * commit/rollback is handled in this case by the parent transaction block.
   */
  def inTransaction[A](a: =>A): A =
    if(! Session.hasCurrentSession)
      _executeTransactionWithin(SessionFactory.newSession, a _)
    else {
      val s = Session.currentSession
      try {
        a
      }
      finally {
        s.cleanup
      }
    }

  private def _executeTransactionWithin[A](s: Session, a: ()=>A) = {

    val c = s.connection

    if(c.getAutoCommit)
      c.setAutoCommit(false)

    var txOk = false
    try {
      val res = _using(s, a)
      txOk = true
      res
    }
    finally {
      try {
        if(txOk)
          c.commit
        else
          c.rollback
      }
      catch {
        case e:SQLException => {
          if(txOk) throw e // if an exception occured b4 the commit/rollback we don't want to obscure the original exception 
        }
      }
      try{c.close}
      catch {
        case e:SQLException => {
          if(txOk) throw e // if an exception occured b4 the close we don't want to obscure the original exception
        }
      }
    }
  }
  
  implicit def __thisDsl:QueryDsl = this  

  private class QueryElementsImpl(override val whereClause: Option[()=>LogicalBoolean])
    extends QueryElements

  def where(b: =>LogicalBoolean): WhereState =
    new QueryElementsImpl(Some(b _))

  def &[A](i: =>TypedExpressionNode[A]): A =
    FieldReferenceLinker.pushExpressionOrCollectValue[A](i _)
  
  def leftOuterJoin[A](a: A, matchClause: =>ExpressionNode): Option[A] = {
    val im = FieldReferenceLinker.isYieldInspectionMode

    if(im) {
      val joinedTableOrSubquery = FieldReferenceLinker.findOwnerOfSample(a).get
      val oje = new OuterJoinExpression(joinedTableOrSubquery,"left", matchClause)
      joinedTableOrSubquery.outerJoinExpression = Some(oje)
      Some(a)
    }
    else if(a.isInstanceOf[net.sf.cglib.proxy.Factory])
      None
    else
      Some(a)  
  }

  def rightOuterJoin[A,B](a: A, b: B, matchClause: =>ExpressionNode): (Option[A],B) = {
    val im = FieldReferenceLinker.isYieldInspectionMode

    if(im) {
      val joinedTableOrSubquery = FieldReferenceLinker.findOwnerOfSample(a).get
      val oje = new OuterJoinExpression(joinedTableOrSubquery,"right", matchClause)
      joinedTableOrSubquery.outerJoinExpression = Some(oje)
      (Some(a), b)
    }
    else {
      val rA = if(a.isInstanceOf[net.sf.cglib.proxy.Factory]) None else Some(a)
      (rA,b)
    }
  }

  def fullOuterJoin[A,B](a: A, b: B, matchClause: =>ExpressionNode): (Option[A],Option[B]) = {
    val im = FieldReferenceLinker.isYieldInspectionMode

    if(im) {
      val joinedTableOrSubquery = FieldReferenceLinker.findOwnerOfSample(a).get
      val oje = new OuterJoinExpression(joinedTableOrSubquery,"full", matchClause)
      joinedTableOrSubquery.outerJoinExpression = Some(oje)
      val parentQuery = FieldReferenceLinker.inspectedQueryExpressionNode
      parentQuery.tableExpressions.head.isRightJoined = true
      (Some(a), Some(b))
    }
    else {
      val rA = if(a.isInstanceOf[net.sf.cglib.proxy.Factory]) None else Some(a)
      val rB = if(b.isInstanceOf[net.sf.cglib.proxy.Factory]) None else Some(b)
      (rA,rB)
    }
  }
  
  trait SingleRowQuery[R] {
    self: Query[R] =>
  }

  trait SingleColumnQuery[T] {
    self: Query[T] =>
  }

  trait ScalarQuery[T] extends Query[T] with SingleColumnQuery[T] with SingleRowQuery[T]

  implicit def scalarQuery2Scalar[T](sq: ScalarQuery[T]) = sq.head

  implicit def countQueryableToIntTypeQuery[R](q: Queryable[R]) = new CountSubQueryableQuery(q)

  private def _countFunc = count
  
  class CountSubQueryableQuery(q: Queryable[_]) extends Query[LongType] with ScalarQuery[LongType] {

    private val _inner:Query[Measures[LongType]] =
      from(q)(r => compute(_countFunc))

    def iterator = _inner.map(m => m.measures).iterator

    def Count: ScalarQuery[LongType] = this

    def statement: String = _inner.statement

    // Paginating a Count query makes no sense perhaps an error() would be more appropriate here:
    def page(offset:Int, length:Int) = this      

    def distinct = this

    def forUpdate = _inner.forUpdate

    def dumpAst = _inner.dumpAst

    def ast = _inner.ast

    protected[squeryl] def invokeYield(rsm: ResultSetMapper, rs: ResultSet) =
      _inner.invokeYield(rsm, rs).measures

    override private[squeryl] def copy(asRoot:Boolean) = new CountSubQueryableQuery(q)

    def name = _inner.name

    private[squeryl] def give(rsm: ResultSetMapper, rs: ResultSet) =
      q.invokeYield(rsm, rs)
  }

  implicit def singleColComputeQuery2ScalarQuery[T](cq: Query[Measures[T]]): ScalarQuery[T] = new ScalarMeasureQuery[T](cq)
  
  implicit def singleColComputeQuery2Scalar[T](cq: Query[Measures[T]]) = new ScalarMeasureQuery[T](cq).head

  class ScalarMeasureQuery[T](q: Query[Measures[T]]) extends Query[T] with ScalarQuery[T] {

    def iterator = q.map(m => m.measures).iterator

    def distinct = this

    def forUpdate = q.forUpdate
    
    def dumpAst = q.dumpAst

    // TODO: think about this : Paginating a Count query makes no sense perhaps an error() would be more appropriate here.
    def page(offset:Int, length:Int) = this
    
    def statement: String = q.statement
    
    def ast = q.ast

    protected[squeryl] def invokeYield(rsm: ResultSetMapper, rs: ResultSet) =
      q.invokeYield(rsm, rs).measures

    override private[squeryl] def copy(asRoot:Boolean) = new ScalarMeasureQuery(q)

    def name = q.name

    private[squeryl] def give(rsm: ResultSetMapper, rs: ResultSet) =
      q.invokeYield(rsm, rs).measures
  }

  implicit def queryable2OptionalQueryable[A](q: Queryable[A]) = new OptionalQueryable[A](q)

  def update[A](t: Table[A])(s: A =>UpdateStatement):Int = t.update(s)

}
