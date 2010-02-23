package org.squeryl.dsl

import ast._
import boilerplate._
import fsm.{QueryElements, StartState, WhereState}
import org.squeryl.internals._
import java.sql.ResultSet
import org.squeryl._

trait QueryDsl
  extends DslFactory
  with WhereState
  with ComputeMeasuresSignaturesFromStartOrWhereState
  with StartState
  with QueryElements
  with FromSignatures {

  def using[A](session: Session)(a: =>A): A = {
    session.bindToCurrentThread
    val r = a
    session.unbindFromCurrentThread
    r
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
