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
  with JoinSignatures
  with FromSignatures {
  outerQueryDsl =>
  
  def using[A](session: Session)(a: =>A): A =
    _using(session, a _)

  private def _using[A](session: Session, a: ()=>A): A =
    try {
      session.bindToCurrentThread
      val r = a()
      r
    }
    finally {
      session.unbindFromCurrentThread
      session.cleanup
    }

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

  @deprecated("use the new 'join' keyword instead http://squeryl.org/joins.html")
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

  @deprecated("use the new 'join' keyword instead http://squeryl.org/joins.html")
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

  @deprecated("use the new 'join' keyword instead http://squeryl.org/joins.html")
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

  implicit def view2QueryAll[A](v: View[A]) = from(v)(a=> select(a))

  def update[A](t: Table[A])(s: A =>UpdateStatement):Int = t.update(s)

  def manyToManyRelation[L <: KeyedEntity[_],R <: KeyedEntity[_],A <: KeyedEntity[_]](l: Table[L], r: Table[R]) = new ManyToManyRelationBuilder(l,r)

  class ManyToManyRelationBuilder[L <: KeyedEntity[_], R <: KeyedEntity[_]](l: Table[L], r: Table[R]) {

    def via[A <: KeyedEntity[_]](f: (L,R,A)=>Pair[EqualityExpression,EqualityExpression])(implicit manifestA: Manifest[A], schema: Schema) = {
      val m2m = new ManyToManyRelationImpl(l,r,manifestA.erasure.asInstanceOf[Class[A]],f,schema)
      schema._addTable(m2m)
      m2m
    }
  }

  class ManyToManyRelationImpl[L <: KeyedEntity[_], R <: KeyedEntity[_], A <: KeyedEntity[_]](val leftTable: Table[L], val rightTable: Table[R], aClass: Class[A], f: (L,R,A)=>Pair[EqualityExpression,EqualityExpression], schema: Schema)
    extends Table[A](schema.tableNameFromClass(aClass), aClass, schema, None) with ManyToManyRelation[L,R,A] {
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
      ee.filterDescendantsOfType[SelectElementReference[Any]].filter(
        _.selectElement.origin.asInstanceOf[ViewExpressionNode[_]].view == v
      ).headOption != None

    /**
     * returns a (FieldMetaData, FieldMetaData) where ._1 is the id of the KeyedEntity on the left or right side,
     * and where ._2 is the foreign key of the association object/table
     */
    private def _splitEquality(ee: EqualityExpression) =
      if(ee.left._fieldMetaData.parentMetaData.clasz == aClass) {
        assert(ee.right._fieldMetaData.isIdFieldOfKeyedEntity)
        (ee.right._fieldMetaData, ee.left._fieldMetaData)
      }
      else {
        assert(ee.left._fieldMetaData.isIdFieldOfKeyedEntity)
        (ee.left._fieldMetaData, ee.right._fieldMetaData)
      }

    private val (leftPkFmd, leftFkFmd) = _splitEquality(_leftEqualityExpr)

    private val (rightPkFmd, rightFkFmd) = _splitEquality(_rightEqualityExpr)

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

        def assign(o: R, a: A) =
          _assignKeys(o, a.asInstanceOf[AnyRef])
        
        def associate(o: R, a: A): Unit  = {
          assign(o, a)
          thisTableOfA.insertOrUpdate(a)
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

        def assign(o: L, a: A) =
          _assignKeys(o, a.asInstanceOf[AnyRef])
        
        def associate(o: L, a: A): Unit = {
          assign(o, a)
          thisTableOfA.insertOrUpdate(a)
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
    
    private val (_leftPkFmd, _rightFkFmd) = {

      var ee: Option[EqualityExpression] = None

      from(leftTable,rightTable)((o,m) => {
        ee = Some(f(o,m))
        select(None)
      })

      val ee_ = ee.get
      
      (ee_.left.asInstanceOf[SelectElementReference[_]].selectElement.asInstanceOf[FieldSelectElement].fieldMataData,
       ee_.right.asInstanceOf[SelectElementReference[_]].selectElement.asInstanceOf[FieldSelectElement].fieldMataData)
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
        }

        def delete =
          leftTable.deleteWhere(o => f(o, rightSide)) > 0
      }
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
}
