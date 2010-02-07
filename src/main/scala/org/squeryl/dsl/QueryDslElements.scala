package org.squeryl.dsl

import ast._
import boilerplate._
import org.squeryl.internals._
import java.sql.ResultSet
import org.squeryl._

import UtilZZZ._
  
class QueryDslElements

trait GroupBySignatures {
  self: QueryElements =>

  def GroupBy[T1](e1: =>GroupArg[T1]): GroupByState[T1] =
    new GroupQueryYield[T1](this,
      ()=>List(e1)
    )

  def GroupBy[T1,T2](e1: =>GroupArg[T1], e2: =>GroupArg[T2]): GroupByState[(T1,T2)] =
    new GroupQueryYield[(T1,T2)](this,
      ()=>List(e1, e2)
    )

  def GroupBy[T1,T2,T3](e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3]): GroupByState[(T1,T2,T3)] =
    new GroupQueryYield[(T1,T2,T3)](this,
      ()=>List(e1, e2, e3)
    )

  def GroupBy[T1,T2,T3,T4](e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4]): GroupByState[(T1,T2,T3,T4)] =
    new GroupQueryYield[(T1,T2,T3,T4)](this,
      ()=>List(e1, e2, e3, e4)
    )

  def GroupBy[T1,T2,T3,T4,T5](e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4], e5: =>GroupArg[T5]): GroupByState[(T1,T2,T3,T4,T5)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5)](this,
      ()=>List(e1, e2, e3, e4, e5)
    )

  def GroupBy[T1,T2,T3,T4,T5,T6]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5], e6: =>GroupArg[T6]):
     GroupByState[(T1,T2,T3,T4,T5,T6)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6)](this,
      ()=>List(e1, e2, e3, e4, e5, e6)
    )

  def GroupBy[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5], e6: =>GroupArg[T6], e7: =>GroupArg[T7]):
     GroupByState[(T1,T2,T3,T4,T5,T6,T7)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6,T7)](this,
      ()=>List(e1, e2, e3, e4, e5, e6, e7)
    )

  def GroupBy[T1,T2,T3,T4,T5,T6,T7,T8]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5], e6: =>GroupArg[T6], e7: =>GroupArg[T7], e8: =>GroupArg[T8]):
     GroupByState[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    new GroupQueryYield[(T1,T2,T3,T4,T5,T6,T7,T8)](this,
      ()=>List(e1, e2, e3, e4, e5, e6, e7, e8)
    )
}

trait ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>

  def Compute[T1](e1: =>ComputeArg[T1]): ComputeStateFromGroupByState[G,T1] =
    new GroupWithMeasuresQueryYield[G,T1](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1)
    )

  def Compute[T1,T2](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2]): ComputeStateFromGroupByState[G,(T1,T2)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2)
    )

  def Compute[T1,T2,T3](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3]): ComputeStateFromGroupByState[G,(T1,T2,T3)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3)
    )

  def Compute[T1,T2,T3,T4](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4)
    )

  def Compute[T1,T2,T3,T4,T5]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5)
    )

  def Compute[T1,T2,T3,T4,T5,T6]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def Compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6], e7: =>ComputeArg[T7]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6,T7)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6,T7)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}

trait ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements =>

  def Compute[T1](e1: =>ComputeArg[T1]): ComputeStateStartOrWhereState[T1] =
    new MeasuresQueryYield[T1](
      this,
      () =>List(e1)
    )

  def Compute[T1,T2](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2]): ComputeStateStartOrWhereState[(T1,T2)] =
    new MeasuresQueryYield[(T1,T2)](
      this,
      () =>List(e1, e2)
    )

  def Compute[T1,T2,T3](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3]): ComputeStateStartOrWhereState[(T1,T2,T3)] =
    new MeasuresQueryYield[(T1,T2,T3)](
      this,
      () =>List(e1, e2, e3)
    )

  def Compute[T1,T2,T3,T4]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4]): ComputeStateStartOrWhereState[(T1,T2,T3,T4)] =
    new MeasuresQueryYield[(T1,T2,T3,T4)](
      this,
      () =>List(e1, e2, e3, e4)
    )

  def Compute[T1,T2,T3,T4,T5]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5)](
      this,
      () =>List(e1, e2, e3, e4, e5)
    )

  def Compute[T1,T2,T3,T4,T5,T6]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5,T6)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5,T6)](
      this,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def Compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6], e7: =>ComputeArg[T7]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5,T6,T7)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5,T6,T7)](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}

trait ComputeStateStartOrWhereState[M]
  extends QueryYield[Measures[M]]
    with OrderBySignatures[Measures[M]] {

  self: MeasuresQueryYield[M] =>
}

trait WhereState extends GroupBySignatures {
  self: QueryElements =>

  def Select[R](yieldClosure: =>R): SelectState[R] =
    new QueryYieldImpl[R](this, yieldClosure _)

  def Set(updateAssignments: UpdateAssignment*) =
    new UpdateStatement(_whereClause, updateAssignments )
}

trait OrderBySignatures[R] {
  self: QueryYieldImpl[R] =>

  type O = OrderByArg

  def OrderBy(e1: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O, e3: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O, e6: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _, e6 _)
    this
  }

  def OrderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O, e6: =>O, e7: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _, e6 _, e7 _)
    this
  }
}

trait HavingState[G]
    extends ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>
}

trait ComputeStateFromGroupByState[K,M]
  extends QueryYield[GroupWithMeasures[K,M]] with OrderBySignatures[GroupWithMeasures[K,M]] {
  self: GroupWithMeasuresQueryYield[K,M] =>
}


trait GroupByState[K]
  extends QueryYield[Group[K]]
  with ComputeMeasuresSignaturesFromGroupByState[K]
  with OrderBySignatures[Group[K]] {
  self: GroupQueryYield[K] =>

  def Having(b: =>TypedExpressionNode[Agregate,LogicalBoolean]): HavingState[K] = {
    _havingClause = Some(b _)
    this
  }
}

trait SelectState[R] extends QueryYield[R] with OrderBySignatures[R] {
  self: QueryYieldImpl[R] =>
}

trait StartState
  extends GroupBySignatures
  with ComputeMeasuresSignaturesFromStartOrWhereState {

  self: QueryElements =>

  def Where(b: =>TypedExpressionNode[Scalar,LogicalBoolean]): WhereState

  def Select[R](yieldClosure: =>R): SelectState[R]
}

class QueryElementsImpl(wc: ()=>TypedExpressionNode[Scalar,LogicalBoolean]) extends QueryElements {

  override val _whereClause = Some(wc)

  override def Where(b: =>TypedExpressionNode[Scalar,LogicalBoolean]) =
    error("the type system should not allow this method to be invoked, it is a bug or instanceOf has been misused.") 
}

trait QueryElements
  extends WhereState
    with ComputeMeasuresSignaturesFromStartOrWhereState
    with StartState {

  def _whereClause:Option[()=>TypedExpressionNode[Scalar,LogicalBoolean]] = None
}


class QueryYieldImpl[G]
  (val queryElementzz: QueryElements, val yieldClosure: ()=>G)
  extends SelectState[G]
    with OrderBySignatures[G]
    with QueryYield[G] {

  var _havingClause: Option[()=>TypedExpressionNode[Agregate,LogicalBoolean]] = None

  //TODO: an array is probably more efficient, even if less 'lazy' :
  var _orderByExpressions: () => List[()=>ExpressionNode] = null

  def whereClause: Option[ExpressionNode] =
    queryElementzz._whereClause.map(b=>b())

  def havingClause: Option[ExpressionNode] =
    _havingClause.map(c=>c())

  def groupByClause: Iterable[ExpressionNode] = Iterable.empty

  def queryElements =
    (whereClause, havingClause, groupByClause, orderByClause)

  def computeClause:List[ExpressionNode] = List.empty

  def orderByClause: Iterable[ExpressionNode] = {
    if(_orderByExpressions == null)
      List.empty
    else
      _orderByExpressions().map(c=>c())
  }

  def invokeYield(rsm: ResultSetMapper, rs: ResultSet): G =
    yieldClosure()

  def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper) =
    FieldReferenceLinker.determineColumnsUtilizedInYeldInvocation(
      q, rsm, ()=>invokeYield(rsm, null).asInstanceOf[AnyRef])
}

class GroupQueryYield[K] (
   _qe: QueryElements,
   val groupByClauseClosure: ()=>List[GroupArg[_]]
  )
  extends QueryYieldImpl[Group[K]](_qe, null)
    with GroupByState[K]
    with HavingState[K]
    with OrderBySignatures[Group[K]]
    with QueryYield[Group[K]]
{

  override def groupByClause: List[ExpressionNode] =
    groupByClauseClosure().map(e => e.expression)

  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet): Group[K] =
    new Group(rsm.groupKeysMapper.get.mapToTuple(rs))

  override def queryElements =
    (whereClause, havingClause, groupByClause, orderByClause)

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper) = {
    val offset = 1
    val (m, nodes) = _createColumnToTupleMapper(q, groupByClauseClosure(), offset, true)
    rsm.groupKeysMapper = Some(m)
    val st = new STuple6(nodes, m.outMappers).asInstanceOf[K]
    (nodes, new Group(st))
  }
}

class MeasuresQueryYield[M](
   _qe: QueryElements,
   _computeByClauseClosure: ()=>List[ComputeArg[_]]
  )
  extends QueryYieldImpl[Measures[M]](_qe, null)
    with OrderBySignatures[Measures[M]]
    with ComputeStateStartOrWhereState[M]
    with QueryYield[Measures[M]]
{
  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet): Measures[M] =
    new Measures(rsm.groupMeasuresMapper.get.mapToTuple(rs))

  override def queryElements =
    (whereClause, havingClause, groupByClause, orderByClause)

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper) = {
    val offset = 1
    val (m, nodes) = _createColumnToTupleMapper(q, _computeByClauseClosure(), offset, false)
    rsm.groupMeasuresMapper = Some(m)
    val st = new STuple6(nodes, m.outMappers).asInstanceOf[M]
    (nodes, new Measures(st))
  }
}



object UtilZZZ {
  def _createColumnToTupleMapper(origin: QueryableExpressionNode, agregateArgs: List[AgregateArg], offsetInResultSet:Int, isForGroup:Boolean) = {
  import boilerplate.STuple1
  import boilerplate.STuple3

  var i = -1;
    val nodes = agregateArgs.map(e => { i += 1; new TupleSelectElement(origin, e.expression, i, isForGroup)})

    var o = offsetInResultSet

    val mappers = new Array[OutMapper[_]](agregateArgs.size)

    var k:Int = 0
    agregateArgs.foreach(e => {
      e.mapper.index = o
      o += 1;
      mappers(k) = e.mapper
      k += 1
    })

    val m = new ColumnToTupleMapper(mappers)

    for(n <- nodes)
      n.columnToTupleMapper = Some(m)
    (m, nodes)
  }  
}

class GroupWithMeasuresQueryYield[K,M] (
  _qe: QueryElements,
  _groupByClauseClosure: ()=>List[GroupArg[_]],
  _computeClauseClosure: ()=>List[ComputeArg[_]]
)
extends QueryYieldImpl[GroupWithMeasures[K,M]](_qe, null)
  with ComputeStateFromGroupByState[K,M]
  with OrderBySignatures[GroupWithMeasures[K,M]]
  with QueryYield[GroupWithMeasures[K,M]]
{

   override def queryElements =
    (whereClause, havingClause, _groupByClauseClosure().map(e => e.expression), orderByClause)

  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet) =
    new GroupWithMeasures(rsm.groupKeysMapper.get.mapToTuple(rs), rsm.groupMeasuresMapper.get.mapToTuple(rs))

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper) = {

    val offset = 1

    val (km, knodes) = _createColumnToTupleMapper(q, _groupByClauseClosure(), offset, true)
    val (mm, mnodes) = _createColumnToTupleMapper(q, _computeClauseClosure(), offset + knodes.size, false)

    rsm.groupKeysMapper = Some(km)
    rsm.groupMeasuresMapper = Some(mm)

    val stK = new STuple3(knodes, km.outMappers).asInstanceOf[K]
    val stM = new STuple3(mnodes, mm.outMappers).asInstanceOf[M]

    (List(knodes,mnodes).flatten,  new SampleGroupWithMeasures(stK, stM))
  }
}

class SampleGroupWithMeasures[K, M](k:K, m:M)
  extends GroupWithMeasures(k,m) {

  override def key =
    k match {
      case t:STuple1[_] =>
        if(t.productArity == 1)
          t._1.asInstanceOf[K]
        else k
    }

  override def measures =
    m match {
      case t:STuple1[_] =>
        if(t.productArity == 1)
          t._1.asInstanceOf[M]
        else m
    }
}



trait AgregateArg {
  def expression: ExpressionNode
  def mapper: OutMapper[_]
}

class GroupArg[T](val expression: TypedExpressionNode[Scalar,T], val mapper: OutMapper[T]) extends AgregateArg

//TODO: replace Scalar with Agregate when Scalac can handle disambiguation on context
class ComputeArg[T](val expression: TypedExpressionNode[Agregate,T], val mapper: OutMapper[T]) extends AgregateArg


class OrderByArg(e: TypedExpressionNode[Scalar,_]) extends ExpressionNode {

  override def inhibited = e.inhibited

  def doWrite(sw: StatementWriter) = {
    e.write(sw)
    if(isAscending)
      sw.write(" Asc")
    else
      sw.write(" Desc")
  }

  override def children = List(e)

  private var _ascending = true

  private [squeryl] def isAscending = _ascending

  def Asc = {
    _ascending = true
    this
  }

  def Desc = {
    _ascending = false
    this
  }
}
