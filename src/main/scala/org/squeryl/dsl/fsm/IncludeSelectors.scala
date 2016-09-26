package org.squeryl.dsl.fsm

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.dsl.ast.EqualityExpression
import org.squeryl.dsl.internal.{OuterJoinedQueryable, JoinedQueryable}
import org.squeryl.internals.FieldReferenceLinker

import scala.reflect.ClassTag

trait IncludePathRelation {
  def relationshipAccessor[T](o: Any): T
  def right: IncludePathCommon
  def equalityExpressionAccessor(m: Any, o: Any): EqualityExpression
  def inhibited: Boolean = _inhibitedByWhen

  private [squeryl] var _inhibitedByWhen = false

  protected def getUnderlyingValue(o: Any): Any = {
    if(o.isInstanceOf[Option[Any]])
      getUnderlyingValue(o.asInstanceOf[Option[Any]].get)
    else
      o
  }
}

trait IncludePathCommon {
  def classType: ClassTag[_]
  def relations: Seq[IncludePathRelation] = _relations
  var _relations: Seq[IncludePathRelation] = Seq()
  def table: Table[_]
  def schema: Schema

  protected def tableFor[A]()(implicit s: Schema, aClass: ClassTag[A]): Table[A] = {
    s.findAllTablesFor(aClass.runtimeClass).head.asInstanceOf[Table[A]]
  }

  protected def sampleFor[A](a: Table[A])(implicit s: Schema): A = {
    val v = a.asInstanceOf[View[A]]
    val vxn = v.viewExpressionNode
    vxn.sample =
      v.posoMetaData.createSample(FieldReferenceLinker.createCallBack(vxn))

    vxn.sample
  }
}

class PathBuilder[P](head: IncludePathCommon, allRelations: Seq[IncludePathRelation])
  extends IncludePathCommon {

  private val lastPath = allRelations.lastOption.map(_.right).getOrElse(head)

    def -*[A](i: (P) => IncludableOneToMany[A])(implicit s: Schema, mClass: ClassTag[P], pClass: ClassTag[A]): PathBuilder[A] = {
      val rightNode = new IncludePathNode[A]()
      val relation = new OneToManyIncludePathRelation[P, A](i, rightNode)

      lastPath._relations ++= Seq(relation)

      new PathBuilder[A](head, allRelations ++ Seq(relation))
    }
  
    def *-[A](i: (P) => IncludableManyToOne[A])(implicit s: Schema, mClass: ClassTag[P], pClass: ClassTag[A]): PathBuilder[A] = {
      val rightNode = new IncludePathNode[A]()
      val relation = new ManyToOneIncludePathRelation[P, A](i, rightNode)

      lastPath._relations ++= Seq(relation)

      new PathBuilder[A](head, allRelations ++ Seq(relation))
    }

    def ->>(i: (PathBuilder[P] => PathBuilder[_])*)(implicit schema: Schema, mClass: ClassTag[P]): PathBuilder[_] = {
      i.foreach(_(this))
      this
    }

    def inhibitWhen(inhibited: Boolean): PathBuilder[P] = {
      allRelations.lastOption.foreach(_._inhibitedByWhen = inhibited)
      lastPath.relations.foreach(_._inhibitedByWhen = inhibited)
      this
    }

    def classType: ClassTag[_] = head.classType

    override def relations: Seq[IncludePathRelation] = head.relations

    def table: Table[_] = head.table

    def schema: Schema = head.schema
}

class OneToManyIncludePathRelation[O, M](accessor: O => IncludableOneToMany[M], override val right: IncludePathCommon)(implicit schema: Schema, oClass: ClassTag[O], mClass: ClassTag[M])
  extends IncludePathRelation {
  val table = schema.findAllTablesFor(oClass.runtimeClass).head
  
  def relationshipAccessor[T](o: Any): T = accessor(o.asInstanceOf[O]).asInstanceOf[T]

  def equalityExpressionAccessor(o: Any, m: Any): EqualityExpression =
    schema.findOneToManyRelationsFor(oClass.runtimeClass.asInstanceOf[Class[O]], mClass.runtimeClass.asInstanceOf[Class[Any]]).head.equalityExpression.apply(getUnderlyingValue(o).asInstanceOf[O], getUnderlyingValue(m))

  val joinedQueryable: JoinedQueryable[_] = new OuterJoinedQueryable[Any](table.asInstanceOf[Queryable[Any]], "left")

  def classType: ClassTag[_] = oClass
}

class ManyToOneIncludePathRelation[M, O](accessor: M => IncludableManyToOne[O], override val right: IncludePathCommon)(implicit schema: Schema, mClass: ClassTag[M], oClass: ClassTag[O])
  extends IncludePathRelation {
  val table = schema.findAllTablesFor(oClass.runtimeClass).head
  
  def relationshipAccessor[T](m: Any): T = accessor(m.asInstanceOf[M]).asInstanceOf[T]

  def equalityExpressionAccessor(m: Any, o: Any): EqualityExpression =
    schema.findOneToManyRelationsFor(oClass.runtimeClass.asInstanceOf[Class[O]], mClass.runtimeClass.asInstanceOf[Class[Any]]).head.equalityExpression.apply(getUnderlyingValue(o).asInstanceOf[O], getUnderlyingValue(m))

  val joinedQueryable: JoinedQueryable[_] = new OuterJoinedQueryable[Any](table.asInstanceOf[Queryable[Any]], "left")

  def classType: ClassTag[_] = oClass
}

class IncludePathNode[M]()(implicit override val schema: Schema, mClass: ClassTag[M])
  extends IncludePathCommon {
  val table = tableFor[M]()
  private val sample = sampleFor[M](table)

  def classType: ClassTag[_] = mClass
}
