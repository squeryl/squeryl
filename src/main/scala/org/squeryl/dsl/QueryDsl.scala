package org.squeryl.dsl

import ast._
import boilerplate._
import org.squeryl.internals._
import java.sql.ResultSet
import org.squeryl._

trait QueryDsl
  extends DslFactory
  with FromSignatures {

  implicit def __thisDsl:QueryDsl = this  
  
  def ~: : StartState = new QueryElements

  implicit val _sampleScalarInt: ScalarInt = new ConstantExpressionNode(sampleInt) with ScalarInt
  implicit val _sampleScalarString: ScalarString = new ConstantExpressionNode(sampleString) with ScalarString
  //implicit val _sampleScalarStringOption: ScalarStringOption = new ConstantExpressionNode(sampleString) with ScalarStringOption
  implicit val _sampleScalarDouble: ScalarDouble = new ConstantExpressionNode(sampleDouble) with ScalarDouble
  implicit val _sampleScalarFloat: ScalarFloat = new ConstantExpressionNode(sampleFloat) with ScalarFloat

  def Value(i: =>ScalarInt)(implicit si: ScalarInt): IntType =
    FieldReferenceLinker.pushExpressionOrCollectValue[IntType](i _, sampleInt)

  def Value(i: =>ScalarString)(implicit si: ScalarString): StringType =
    FieldReferenceLinker.pushExpressionOrCollectValue[StringType](i _, sampleString)

//  def Value(i: =>ScalarStringOption)(implicit si: ScalarStringOption): Option[StringType] =
//    FieldReferenceLinker.pushExpressionOrCollectValue[Option[StringType]](i _, Some(sampleString))

  def Value(i: =>ScalarDouble)(implicit si: ScalarDouble): DoubleType =
    FieldReferenceLinker.pushExpressionOrCollectValue[DoubleType](i _, sampleDouble)

  def Value(i: =>ScalarFloat)(implicit si: ScalarFloat): FloatType =
    FieldReferenceLinker.pushExpressionOrCollectValue[FloatType](i _, sampleFloat)

  def OuterJoin[A](a: A, j: =>LeftOuterJoinNode): Option[A] = {

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

  def OuterJoin[A,B](a: A, b: B, j: =>FullOuterJoinNode): (Option[A],Option[B]) = {
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

    def Having(b: =>AgregateLogicalBoolean): HavingState[K] = {
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

    def Where(b: =>ScalarLogicalBoolean): WhereState

    def Select[R](yieldClosure: =>R): SelectState[R]
  }

  class QueryElements
    extends WhereState
      with ComputeMeasuresSignaturesFromStartOrWhereState
      with StartState {
    
    var _whereClause: Option[()=>ScalarLogicalBoolean] = None

    def Where(b: =>ScalarLogicalBoolean): WhereState = {
      _whereClause = Some(b _)
      this
    }
  }

  class QueryYieldImpl[G]
    (val queryElementzz: QueryElements, val yieldClosure: ()=>G)
    extends SelectState[G]
      with OrderBySignatures[G]
      with QueryYield[G] {

    var _havingClause: Option[()=>AgregateLogicalBoolean] = None

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
  
  private def _createColumnToTupleMapper(origin: QueryableExpressionNode, agregateArgs: List[AgregateArg], offsetInResultSet:Int, isForGroup:Boolean) = {
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

  private def _countFunc = Count

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

    private val _inner:Query[Measures[LongType]] = From(q)(r =>
      ~:Compute(new ComputeArg[LongType](_countFunc, createOutMapperLongType)))

    def iterator = _inner.map(m => m.measures).iterator

    def Count: ScalarQuery[LongType] = this

    def statement: String = _inner.statement

    def Distinct = this

    def ForUpdate = _inner.ForUpdate

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

    def Distinct = this

    def ForUpdate = q.ForUpdate
    
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

  // TODO: put this in Queryable .... with implicit dsl...
  class QueryableView[T](q: Queryable[T]) {
    def where(whereClauseFunctor: T => ScalarLogicalBoolean): Query[T] =
      From(q)(q0 =>
      ~:Where(whereClauseFunctor(q0))
        Select(q0)
      )
  }

  implicit def queryable2QueryableView[T](q: Queryable[T]) = new QueryableView[T](q)

  implicit def queryable2OptionalQueryable[A](q: Queryable[A]) = new OptionalQueryable[A](q)
}
