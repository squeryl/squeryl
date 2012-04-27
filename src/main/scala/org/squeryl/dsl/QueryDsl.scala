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

import ast._
import boilerplate._
import fsm._
import org.squeryl.internals._
import org.squeryl._
import java.sql.{SQLException, ResultSet}
import collection.mutable.ArrayBuffer
import scala.runtime.NonLocalReturnControl

trait QueryDsl
  extends WhereState[Unconditioned]
  with ComputeMeasuresSignaturesFromStartOrWhereState
  with StartState
  with QueryElements[Unconditioned]
  with JoinSignatures
  with FromSignatures {
  outerQueryDsl =>
  
  implicit def queryToIterable[R](q: Query[R]): Iterable[R] = {
    
    val i = q.iterator
                
    new Iterable[R] {

      val hasFirst = i.hasNext
                  
      lazy val firstRow = 
        if(hasFirst) Some(i.next) else None    
      
      override def head = firstRow.get
      
      override def headOption = firstRow
      
      override def isEmpty = ! hasFirst
      
      def iterator = 
        new IteratorConcatenation(firstRow.iterator, i)
      
    }
  }
  
//  implicit def viewToIterable[R](t: View[R]): Iterable[R] = 
//      queryToIterable(view2QueryAll(t))
  

  def using[A](session: Session)(a: =>A): A =
    _using(session, a _)

  private def _using[A](session: Session, a: ()=>A): A = {
    val s = Session.currentSessionOption
    try {
      if(s != None) s.get.unbindFromCurrentThread
      try {
        session.bindToCurrentThread
        val r = a()
        r
      }
      finally {
        session.unbindFromCurrentThread
        session.cleanup
      }
    }
    finally {
      if(s != None) s.get.bindToCurrentThread
    }
  }

  def transaction[A](sf: SessionFactory)(a: =>A) = 
    _executeTransactionWithin(sf.newSession, a _)
  
  def inTransaction[A](sf: SessionFactory)(a: =>A) =
    if(! Session.hasCurrentSession)
      _executeTransactionWithin(sf.newSession, a _)
    else
      _executeTransactionWithin(Session.currentSession, a _)

   def transaction[A](s: Session)(a: =>A) = 
     _executeTransactionWithin(s, a _)
   
  /**
   * 'transaction' causes a new transaction to begin and commit after the block execution, or rollback
   * if an exception occurs. Invoking a transaction always cause a new one to
   * be created, even if called in the context of an existing transaction.
   */
  def transaction[A](a: =>A): A =
    if(! Session.hasCurrentSession)
      _executeTransactionWithin(SessionFactory.newSession, a _)
    else {
      val s = Session.currentSession
      val res =
        try {
          s.unbindFromCurrentThread
          _executeTransactionWithin(SessionFactory.newSession, a _)
        }
        finally {
          s.bindToCurrentThread
        }
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
      a
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
    catch {
      case e:NonLocalReturnControl[_] => 
      {
        txOk = true
        throw e
      }
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
          Utils.close(c)
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

  private class QueryElementsImpl[Cond](override val whereClause: Option[()=>LogicalBoolean])
    extends QueryElements[Cond]

  def where(b: =>LogicalBoolean): WhereState[Conditioned] =
    new QueryElementsImpl[Conditioned](Some(b _))

  def &[A,T](i: =>TypedExpression[A,T]): A =
    FieldReferenceLinker.pushExpressionOrCollectValue[A](i _)
    
  
  implicit def typedExpression2OrderByArg[E <% TypedExpression[_,_]](e: E) = new OrderByArg(e)

  implicit def orderByArg2OrderByExpression(a: OrderByArg) = new OrderByExpression(a)

  def sDevPopulation[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("stddev_pop", Seq(b)))
         
  def sDevSample[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("stddev_samp", Seq(b)))
         
  def varPopulation[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("var_pop", Seq(b)))
         
  def varSample[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("var_samp", Seq(b)))
  
  def max[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("max", Seq(b)))

  def min[T2 >: TOption, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("min", Seq(b)))

  def avg[T2 >: TOptionFloat, T1 <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("avg", Seq(b)))

  def sum[T2 >: TOption, T1 >: TNumericLowerTypeBound <: T2, A1, A2]
         (b: TypedExpression[A1,T1])
         (implicit f: TypedExpressionFactory[A2,T2]) = f.convert(new FunctionNode("sum", Seq(b)))

  def nvl[T4 <: TNonOption,
          T1 >: TOption,
          T3 >: T1,
          T2 <: T3,
          A1,A2,A3]
         (a: TypedExpression[A1,T1],
          b: TypedExpression[A2,T2])
         (implicit d: DeOptionizer[A3,T4,_,T3]): TypedExpression[A3,T4] = new NvlNode(a, d.deOptionizer.convert(b))
  
  def not(b: LogicalBoolean) = new FunctionNode("not", Seq(b)) with LogicalBoolean

  def upper[A1,T1](s: TypedExpression[A1,T1])(implicit f: TypedExpressionFactory[A1,T1], ev2: T1 <:< TOptionString) = 
    f.convert(new FunctionNode("upper", Seq(s)))
  
  def lower[A1,T1](s: TypedExpression[A1,T1])(implicit f: TypedExpressionFactory[A1,T1], ev2: T1 <:< TOptionString) = 
    f.convert(new FunctionNode("lower", Seq(s)))

  def exists[A1](query: Query[A1]) = new ExistsExpression(query.copy(false).ast, "exists")

  def notExists[A1](query: Query[A1]) = new ExistsExpression(query.copy(false).ast, "not exists")
         
  implicit val numericComparisonEvidence   = new CanCompare[TNumeric, TNumeric]         
  implicit val dateComparisonEvidence      = new CanCompare[TOptionDate, TOptionDate]
  implicit val timestampComparisonEvidence = new CanCompare[TOptionTimestamp, TOptionTimestamp]
  implicit val stringComparisonEvidence    = new CanCompare[TOptionString, TOptionString]
  implicit val booleanComparisonEvidence   = new CanCompare[TOptionBoolean, TOptionBoolean]
  implicit val uuidComparisonEvidence      = new CanCompare[TOptionUUID, TOptionUUID]
  implicit def enumComparisonEvidence[A]   = new CanCompare[TEnumValue[A],TEnumValue[A]]
  
  implicit def concatenationConversion[A1,A2,T1,T2](co: ConcatOp[A1,A2,T1,T2]): TypedExpression[String,TString] = 
    new ConcatOperationNode[String,TString](co.a1, co.a2, PrimitiveTypeMode.stringTEF.createOutMapper)
    
  implicit def concatenationConversionWithOption1[A1,A2,T1,T2](co: ConcatOp[Option[A1],A2,T1,T2]): TypedExpression[Option[String],TOptionString] = 
    new ConcatOperationNode[Option[String],TOptionString](co.a1, co.a2, PrimitiveTypeMode.optionStringTEF.createOutMapper)
  
  implicit def concatenationConversionWithOption2[A1,A2,T1,T2](co: ConcatOp[A1,Option[A2],T1,T2]): TypedExpression[Option[String],TOptionString] = 
    new ConcatOperationNode[Option[String],TOptionString](co.a1, co.a2, PrimitiveTypeMode.optionStringTEF.createOutMapper)
  
  implicit def concatenationConversionWithOption3[A1,A2,T1,T2](co: ConcatOp[Option[A1],Option[A2],T1,T2]): TypedExpression[Option[String],TOptionString] = 
    new ConcatOperationNode[Option[String],TOptionString](co.a1, co.a2, PrimitiveTypeMode.optionStringTEF.createOutMapper)
  
  class ConcatOperationNode[A,T](e1: ExpressionNode, e2: ExpressionNode, val mapper: OutMapper[A]) extends BinaryOperatorNode(e1,e2, "||", false) with TypedExpression[A,T] {
    override def doWrite(sw: StatementWriter) =
      sw.databaseAdapter.writeConcatOperator(e1, e2, sw)       
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

  def count: CountFunction = count()

  def count(e: TypedExpression[_,_]*) = new CountFunction(e, false)

  def countDistinct(e: TypedExpression[_,_]*) = new CountFunction(e, true)
  
  class CountFunction(_args: Seq[ExpressionNode], isDistinct: Boolean) 
    extends FunctionNode("count",
      _args match {
        case Nil =>Seq(new TokenExpressionNode("*")) 
        case _   => _args
      }
    )
    with TypedExpression[Long,TLong] {
    
    def mapper = PrimitiveTypeMode.longTEF.createOutMapper    
    
    override def doWrite(sw: StatementWriter) = {

      sw.write(name)
      sw.write("(")

      if(isDistinct)
        sw.write("distinct ")

      sw.writeNodesWithSeparator(args, ",", false)
      sw.write(")")
    }
  }
  
  private def _countFunc = count
  
  class CountSubQueryableQuery(q: Queryable[_]) extends Query[Long] with ScalarQuery[Long] {

    private val _inner:Query[Measures[Long]] =
      from(q)(r => compute(_countFunc))

    def iterator = _inner.map(m => m.measures).iterator

    def Count: ScalarQuery[Long] = this

    def statement: String = _inner.statement

    // Paginating a Count query makes no sense perhaps an org.squeryl.internals.Utils.throwError() would be more appropriate here:
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

    // TODO: think about this : Paginating a Count query makes no sense perhaps an org.squeryl.internals.Utils.throwError() would be more appropriate here.
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

  /**
   * Used for supporting 'inhibitWhen' dynamic queries
   */
  implicit def queryable2OptionalQueryable[A](q: Queryable[A]) = new OptionalQueryable[A](q)

  //implicit def view2QueryAll[A](v: View[A]) = from(v)(a=> select(a))

  def update[A](t: Table[A])(s: A =>UpdateStatement):Int = t.update(s)

  def manyToManyRelation[L <: KeyedEntity[_],R <: KeyedEntity[_],A <: KeyedEntity[_]](l: Table[L], r: Table[R]) = new ManyToManyRelationBuilder(l,r,None)

  def manyToManyRelation[L <: KeyedEntity[_],R <: KeyedEntity[_],A <: KeyedEntity[_]](l: Table[L], r: Table[R], nameOfMiddleTable: String) = new ManyToManyRelationBuilder(l,r,Some(nameOfMiddleTable))

  class ManyToManyRelationBuilder[L <: KeyedEntity[_], R <: KeyedEntity[_]](l: Table[L], r: Table[R], nameOverride: Option[String]) {

    def via[A <: KeyedEntity[_]](f: (L,R,A)=>Pair[EqualityExpression,EqualityExpression])(implicit manifestA: Manifest[A], schema: Schema) = {
      val m2m = new ManyToManyRelationImpl(l,r,manifestA.erasure.asInstanceOf[Class[A]], f, schema, nameOverride)
      schema._addTable(m2m)
      m2m
    }
  }

  class ManyToManyRelationImpl[L <: KeyedEntity[_], R <: KeyedEntity[_], A <: KeyedEntity[_]](val leftTable: Table[L], val rightTable: Table[R], aClass: Class[A], f: (L,R,A)=>Pair[EqualityExpression,EqualityExpression], schema: Schema, nameOverride: Option[String])
    extends Table[A](nameOverride.getOrElse(schema.tableNameFromClass(aClass)), aClass, schema, None) with ManyToManyRelation[L,R,A] {
    thisTableOfA =>    

    def thisTable = thisTableOfA
    
    schema._addRelation(this)
    
    private val (_leftEqualityExpr, _rightEqualityExpr) = {

      var e2: Option[Pair[EqualityExpression,EqualityExpression]] = None

      from(leftTable, rightTable, thisTableOfA)((l,r,a) => {
        e2 = Some(f(l,r,a))
        select(None)
      })
      
      val e2_ = e2.get

      //invert Pair[EqualityExpression,EqualityExpression] if it has been declared in reverse :
      if(_viewReferedInExpression(leftTable, e2_._1)) {
        assert(_viewReferedInExpression(rightTable, e2_._2))
        e2_
      }
      else {
        assert(_viewReferedInExpression(leftTable, e2_._2))
        assert(_viewReferedInExpression(rightTable, e2_._1))
        (e2_._2, e2_._1)
      }
    }

    private def _viewReferedInExpression(v: View[_], ee: EqualityExpression) =
      ee.filterDescendantsOfType[SelectElementReference[Any,Any]].filter(
        _.selectElement.origin.asInstanceOf[ViewExpressionNode[_]].view == v
      ).headOption != None


    private val (leftPkFmd, leftFkFmd) = _splitEquality(_leftEqualityExpr, thisTable, false)

    private val (rightPkFmd, rightFkFmd) = _splitEquality(_rightEqualityExpr, thisTable, false)

    val leftForeignKeyDeclaration =
      schema._createForeignKeyDeclaration(leftFkFmd.columnName, leftPkFmd.columnName)

    val rightForeignKeyDeclaration =
      schema._createForeignKeyDeclaration(rightFkFmd.columnName, rightPkFmd.columnName)
    
    private def _associate[T <: KeyedEntity[_]](o: T, m2m: ManyToMany[T,A]): A = {
      val aInst = m2m.assign(o)
      try {
        thisTableOfA.insertOrUpdate(aInst)
      }
      catch {
        case e:SQLException =>
          if(Session.currentSession.databaseAdapter.isNotNullConstraintViolation(e))
            throw new RuntimeException(
              "the " + 'associate + " method created and inserted association object of type " +
              posoMetaData.clasz.getName + " that has NOT NULL colums, plase use the other signature of " + 'ManyToMany +
              " that takes the association object as argument : associate(o,a) for association objects that have NOT NULL columns", e)
          else
            throw e
      }
    }
    
    def left(leftSideMember: L): Query[R] with ManyToMany[R,A] = {

      val q =
        from(thisTableOfA, rightTable)((a,r) => {
          val matchClause = f(leftSideMember, r, a)
          outerQueryDsl.where(matchClause._1 and matchClause._2).select(r)
        })


      new DelegateQuery(q) with ManyToMany[R,A] {

        private def _assignKeys(r: R, a: AnyRef): Unit = {
          
          val leftPk = leftPkFmd.get(leftSideMember.asInstanceOf[AnyRef])
          val rightPk = rightPkFmd.get(r.asInstanceOf[AnyRef])

          leftFkFmd.set(a, leftPk)
          rightFkFmd.set(a, rightPk)
        }

        def associationMap =
          from(thisTableOfA, rightTable)((a,r) => {
            val matchClause = f(leftSideMember, r, a)
            outerQueryDsl.where(matchClause._1 and matchClause._2).select((r,a))
          })

        def assign(o: R, a: A) = {
          _assignKeys(o, a.asInstanceOf[AnyRef])
          a
        }
        
        def associate(o: R, a: A): A  = {
          assign(o, a)
          thisTableOfA.insertOrUpdate(a)
          a
        }

        def assign(o: R): A = {
          val aInstAny = thisTableOfA._createInstanceOfRowObject
          val aInst = aInstAny.asInstanceOf[A]
          _assignKeys(o, aInstAny)
          aInst
        }

        def associate(o: R): A =
          _associate(o,this)

        def dissociate(o: R) =
          thisTableOfA.deleteWhere(a0 => _whereClauseForAssociations(a0) and _equalityForRightSide(a0, o)) > 0

        def _whereClauseForAssociations(a0: A) = {
          val leftPk = leftPkFmd.get(leftSideMember.asInstanceOf[AnyRef])
          leftFkFmd.get(a0.asInstanceOf[AnyRef])
          FieldReferenceLinker.createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(leftPk)
        }

        def _equalityForRightSide(a0: A, r: R) = {
          val rightPk = rightPkFmd.get(r.asInstanceOf[AnyRef])
          rightFkFmd.get(a0.asInstanceOf[AnyRef])
          FieldReferenceLinker.createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(rightPk)
        }

        def dissociateAll = 
          thisTableOfA.deleteWhere(a0 => _whereClauseForAssociations(a0))

        def associations =
          thisTableOfA.where(a0 => _whereClauseForAssociations(a0))                  
      }
    }

    def right(rightSideMember: R): Query[L] with ManyToMany[L,A] = {
      val q =
        from(thisTableOfA, leftTable)((a,l) => {
           val matchClause = f(l, rightSideMember, a)
           outerQueryDsl.where(matchClause._1 and matchClause._2).select(l)
        })

      new DelegateQuery(q) with ManyToMany[L,A] {

        private def _assignKeys(l: L, a: AnyRef): Unit = {

          val rightPk = rightPkFmd.get(rightSideMember.asInstanceOf[AnyRef])
          val leftPk = leftPkFmd.get(l.asInstanceOf[AnyRef])

          rightFkFmd.set(a, rightPk)
          leftFkFmd.set(a, leftPk)
        }

        def associationMap =
          from(thisTableOfA, leftTable)((a,l) => {
             val matchClause = f(l, rightSideMember, a)
             outerQueryDsl.where(matchClause._1 and matchClause._2).select((l, a))
          })

        def assign(o: L, a: A) = {
          _assignKeys(o, a.asInstanceOf[AnyRef])
          a
        }
        
        def associate(o: L, a: A): A = {
          assign(o, a)
          thisTableOfA.insertOrUpdate(a)
          a
        }

        def assign(o: L): A = {
          val aInstAny = thisTableOfA._createInstanceOfRowObject
          val aInst = aInstAny.asInstanceOf[A]
          _assignKeys(o, aInstAny)
          aInst
        }

        def associate(o: L): A =
          _associate(o,this)

        def dissociate(o: L) =
          thisTableOfA.deleteWhere(a0 => _whereClauseForAssociations(a0) and _leftEquality(o, a0)) > 0

        def _leftEquality(l: L, a0: A) = {
          val leftPk = leftPkFmd.get(l.asInstanceOf[AnyRef])
          leftFkFmd.get(a0.asInstanceOf[AnyRef])
          FieldReferenceLinker.createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(leftPk)
        }

        def _whereClauseForAssociations(a0: A) = {
          val rightPk = rightPkFmd.get(rightSideMember.asInstanceOf[AnyRef])
          rightFkFmd.get(a0.asInstanceOf[AnyRef])
          FieldReferenceLinker.createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(rightPk)
        }

        def dissociateAll =
          thisTableOfA.deleteWhere(a0 => _whereClauseForAssociations(a0))

        def associations =
          thisTableOfA.where(a0 => _whereClauseForAssociations(a0))      
      }
    }
  }

  def oneToManyRelation[O <: KeyedEntity[_],M](ot: Table[O], mt: Table[M]) = new OneToManyRelationBuilder(ot,mt)

  class OneToManyRelationBuilder[O <: KeyedEntity[_],M](ot: Table[O], mt: Table[M]) {
    
    def via(f: (O,M)=>EqualityExpression)(implicit schema: Schema) =
      new OneToManyRelationImpl(ot,mt,f, schema)

  }

  class OneToManyRelationImpl[O <: KeyedEntity[_],M](val leftTable: Table[O], val rightTable: Table[M], f: (O,M)=>EqualityExpression, schema: Schema)
    extends OneToManyRelation[O,M] {

    schema._addRelation(this)

    private def _isSelfReference =
      leftTable == rightTable

    //we obtain the FieldMetaDatas from the 'via' function by creating an EqualityExpression AST and then extract the FieldMetaDatas from it,
    // the FieldMetaData will serve to set fields (primary and foreign keys on the objects in the relation) 
    private val (_leftPkFmd, _rightFkFmd) = {

      var ee: Option[EqualityExpression] = None
      
      //we create a query for the sole purpose of extracting the equality (inside the relation's 'via' clause)
      from(leftTable,rightTable)((o,m) => {
        ee = Some(f(o,m))
        select(None)
      })

      val ee_ = ee.get  //here we have the equality AST (_ee) contains a left and right node, SelectElementReference
      //that refer to FieldSelectElement, who in turn refer to the FieldMetaData

      // now the Tuple with the left and right FieldMetaData 
      _splitEquality(ee.get, rightTable, _isSelfReference)
    }

    val foreignKeyDeclaration =
      schema._createForeignKeyDeclaration(_rightFkFmd.columnName, _leftPkFmd.columnName)
    
    def left(leftSide: O): OneToMany[M] = {
          
      val q = from(rightTable)(m => where(f(leftSide, m)) select(m))

      new DelegateQuery(q) with OneToMany[M] {

        def deleteAll =
          rightTable.deleteWhere(m => f(leftSide, m))

        def assign(m: M) = {
          val m0 = m.asInstanceOf[AnyRef]
          val l0 = leftSide.asInstanceOf[AnyRef]
          
          val v = _leftPkFmd.get(l0)
          _rightFkFmd.set(m0, v)
          m
        }

        def associate(m: M)(implicit ev: M <:< KeyedEntity[_]) = {
          assign(m)
          rightTable.insertOrUpdate(m)
        }
      }
    }

    def right(rightSide: M): ManyToOne[O] = {

      val q = from(leftTable)(o => where(f(o,rightSide)) select(o))

      new DelegateQuery(q) with ManyToOne[O] {

        def assign(one: O) = {
          val o = one.asInstanceOf[AnyRef]
          val r = rightSide.asInstanceOf[AnyRef]

          val v = _rightFkFmd.get(r)
          _leftPkFmd.set(o, v)
          one
        }

        def delete =
          leftTable.deleteWhere(o => f(o, rightSide)) > 0
      }
    }
  }

  /**
   * returns a (FieldMetaData, FieldMetaData) where ._1 is the id of the KeyedEntity on the left or right side,
   * and where ._2 is the foreign key of the association object/table
   */
  private def _splitEquality(ee: EqualityExpression, rightTable: Table[_], isSelfReference: Boolean):(FieldMetaData,FieldMetaData) = {

    if(isSelfReference)
      assert(ee.right._fieldMetaData.isIdFieldOfKeyedEntity || ee.left._fieldMetaData.isIdFieldOfKeyedEntity)

    if(ee.left._fieldMetaData.parentMetaData.clasz == rightTable.classOfT &&
       (!isSelfReference || (isSelfReference && ee.right._fieldMetaData.isIdFieldOfKeyedEntity)) ) {
      assert(ee.right._fieldMetaData.isIdFieldOfKeyedEntity)
      (ee.right._fieldMetaData, ee.left._fieldMetaData)
    }
    else {
      assert(ee.left._fieldMetaData.isIdFieldOfKeyedEntity)
      (ee.left._fieldMetaData, ee.right._fieldMetaData)
    }
  }

  // Composite key syntactic sugar :

  def compositeKey[A1,A2](a1: A1, a2: A2) =
    new CompositeKey2(a1, a2)

  def compositeKey[A1,A2,A3](a1: A1, a2: A2, a3: A3) =
    new CompositeKey3(a1, a2, a3)

  def compositeKey[A1,A2,A3,A4](a1: A1, a2: A2, a3: A3, a4: A4) =
    new CompositeKey4(a1, a2, a3, a4)

  def compositeKey[A1,A2,A3,A4,A5](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5) =
    new CompositeKey5(a1, a2, a3, a4, a5)

  def compositeKey[A1,A2,A3,A4,A5,A6](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6) =
    new CompositeKey6(a1, a2, a3, a4, a5, a6)

  def compositeKey[A1,A2,A3,A4,A5,A6,A7](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7) =
    new CompositeKey7(a1, a2, a3, a4, a5, a6, a7)

  def compositeKey[A1,A2,A3,A4,A5,A6,A7,A8](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8) =
    new CompositeKey8(a1, a2, a3, a4, a5, a6, a7, a8)

  def compositeKey[A1,A2,A3,A4,A5,A6,A7,A8,A9](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9) =
    new CompositeKey9(a1, a2, a3, a4, a5, a6, a7, a8, a9)

  // Tuple to composite key conversions :
  
  implicit def t2te[A1,A2](t: (A1,A2)) = new CompositeKey2[A1,A2](t._1, t._2)

  implicit def t3te[A1,A2,A3](t: (A1,A2,A3)) = new CompositeKey3[A1,A2,A3](t._1, t._2, t._3)

  implicit def t4te[A1,A2,A3,A4](t: (A1,A2,A3,A4)) = new CompositeKey4[A1,A2,A3,A4](t._1, t._2, t._3, t._4)

  implicit def t5te[A1,A2,A3,A4,A5](t: (A1,A2,A3,A4,A5)) = new CompositeKey5[A1,A2,A3,A4,A5](t._1, t._2, t._3, t._4, t._5)

  implicit def t6te[A1,A2,A3,A4,A5,A6](t: (A1,A2,A3,A4,A5,A6)) = new CompositeKey6[A1,A2,A3,A4,A5,A6](t._1, t._2, t._3, t._4, t._5, t._6)

  implicit def t7te[A1,A2,A3,A4,A5,A6,A7](t: (A1,A2,A3,A4,A5,A6,A7)) = new CompositeKey7[A1,A2,A3,A4,A5,A6,A7](t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  implicit def t8te[A1,A2,A3,A4,A5,A6,A7,A8](t: (A1,A2,A3,A4,A5,A6,A7,A8)) = new CompositeKey8[A1,A2,A3,A4,A5,A6,A7,A8](t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

  implicit def t9te[A1,A2,A3,A4,A5,A6,A7,A8,A9](t: (A1,A2,A3,A4,A5,A6,A7,A8,A9)) = new CompositeKey9[A1,A2,A3,A4,A5,A6,A7,A8,A9](t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

  // Case statements :
/*
  def caseOf[A](expr: NumericalExpression[A]) = new CaseOfNumericalExpressionMatchStart(expr)

  def caseOf[A](expr: NonNumericalExpression[A]) = new CaseOfNonNumericalExpressionMatchStart(expr)

  def caseOf = new CaseOfConditionChainStart
*/
}
