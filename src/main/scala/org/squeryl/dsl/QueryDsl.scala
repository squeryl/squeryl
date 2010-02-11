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

  implicit def __thisDsl:QueryDsl = this  

  private class QueryElementsImpl(override val whereClause: Option[()=>TypedExpressionNode[LogicalBoolean]])
    extends QueryElements

  def where(b: =>TypedExpressionNode[LogicalBoolean]): WhereState =
    new QueryElementsImpl(Some(b _))
  

  implicit val _sampleScalarInt: TypedExpressionNode[Int]= new ConstantExpressionNode(sampleInt) with TypedExpressionNode[Int]
//  implicit val _sampleScalarString: ScalarString = new ConstantExpressionNode(sampleString) with ScalarString
//  //implicit val _sampleScalarStringOption: ScalarStringOption = new ConstantExpressionNode(sampleString) with ScalarStringOption
//  implicit val _sampleScalarDouble: ScalarDouble = new ConstantExpressionNode(sampleDouble) with ScalarDouble
//  implicit val _sampleScalarFloat: ScalarFloat = new ConstantExpressionNode(sampleFloat) with ScalarFloat
//  implicit val _sampleLongOption: ScalarLongOption = new ConstantExpressionNode(Some(sampleLong)) with ScalarLongOption

  def &(i: =>TypedExpressionNode[Int])(implicit si: TypedExpressionNode[Int]): IntType =
    FieldReferenceLinker.pushExpressionOrCollectValue[IntType](i _, sampleInt)

  def &(i: =>ScalarString)(implicit si: ScalarString): StringType =
    FieldReferenceLinker.pushExpressionOrCollectValue[StringType](i _, sampleString)

//  def Value(i: =>ScalarStringOption)(implicit si: ScalarStringOption): Option[StringType] =
//    FieldReferenceLinker.pushExpressionOrCollectValue[Option[StringType]](i _, Some(sampleString))

//  def Value(i: =>ScalarDouble)(implicit si: ScalarDouble): DoubleType =
//    FieldReferenceLinker.pushExpressionOrCollectValue[DoubleType](i _, sampleDouble)
//
//  def Value(i: =>ScalarFloat)(implicit si: ScalarFloat): FloatType =
//    FieldReferenceLinker.pushExpressionOrCollectValue[FloatType](i _, sampleFloat)
//
//  def Value(i: =>ScalarLongOption)(implicit si: ScalarLongOption): Option[LongType] =
//    FieldReferenceLinker.pushExpressionOrCollectValue[Option[LongType]](i _, Some(sampleLong))

  def outerJoin[A](a: A, j: =>LeftOuterJoinNode): Option[A] = {

    val im = FieldReferenceLinker.isYieldInspectionMode
    
    if(im) {
      val on = j
      val leftSelectElement = new SelectElementReference(
        on.left.asInstanceOf[SelectElementReference].selectElement)
      val rightSelectElement = new SelectElementReference(
        on.right.asInstanceOf[SelectElementReference].selectElement)

      on.right.asInstanceOf[SelectElementReference].selectElement.origin.outerJoinColumns =
        Some((leftSelectElement,rightSelectElement,"left"))

      val qen = FieldReferenceLinker.inspectedQueryExpressionNode
      leftSelectElement.parent = Some(qen)
      rightSelectElement.parent = Some(qen)
      Some(a)
    }
    else if(a.isInstanceOf[net.sf.cglib.proxy.Factory])
      None
    else
      Some(a)  
  }

  def outerJoin[A,B](a: A, b: B, j: =>FullOuterJoinNode): (Option[A],Option[B]) = {
    val im = FieldReferenceLinker.isYieldInspectionMode

    if(im) {
      val loj = j
      val leftSelectElement = new SelectElementReference(
        loj.left.asInstanceOf[SelectElementReference].selectElement)
      val rightSelectElement = new SelectElementReference(
        loj.right.asInstanceOf[SelectElementReference].selectElement)

      loj.right.asInstanceOf[SelectElementReference].selectElement.origin.outerJoinColumns =
        Some((rightSelectElement, leftSelectElement,"full"))

      loj.left.asInstanceOf[SelectElementReference].selectElement.origin.fullOuterJoinMatchColumn =
        Some(rightSelectElement)

      val qen = FieldReferenceLinker.inspectedQueryExpressionNode
      leftSelectElement.parent = Some(qen)
      rightSelectElement.parent = Some(qen)
      (Some(a), Some(b))
    }
    else {
      val rA = if(a.isInstanceOf[net.sf.cglib.proxy.Factory]) None else Some(a)
      val rB = if(b.isInstanceOf[net.sf.cglib.proxy.Factory]) None else Some(b)
      (rA,rB)
    }
  }

  private def _countFunc = count

  trait SingleRowQuery[R] {
    self: Query[R] =>
  }

  trait SingleColumnQuery[T] {
    self: Query[T] =>
  }

  trait ScalarQuery[T] extends Query[T] with SingleColumnQuery[T] with SingleRowQuery[T]

  implicit def scalarQuery2Scalar[T](sq: ScalarQuery[T]) = sq.head

  implicit def countQueryableToIntTypeQuery[R](q: Queryable[R]) = new CountSubQueryableQuery(q)

  class CountSubQueryableQuery(q: Queryable[_]) extends Query[LongType] with ScalarQuery[LongType] {

    private val _inner:Query[Measures[LongType]] = from(q)(r =>
      compute(new GroupArg[LongType](_countFunc, createOutMapperLongType)))

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

  //TODO: implement math operators for TypedExpressionNodes (mass * speed ^ 2)
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
