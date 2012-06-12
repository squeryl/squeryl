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

import org.squeryl.dsl.ast._
import org.squeryl.internals._
import org.squeryl.Session
import org.squeryl.Schema
import org.squeryl.internals.AttributeValidOnNumericalColumn
import org.squeryl.Query
import java.util.Date
import java.sql.ResultSet
import org.squeryl.internals.Utils

sealed trait TNumeric
sealed trait TOptionBigDecimal extends TNumeric
sealed trait TBigDecimal extends TOptionBigDecimal with TNonOption

sealed trait TOptionDouble extends TOptionBigDecimal
sealed trait TDouble extends TOptionDouble with TBigDecimal with TNonOption

sealed trait TOptionLong extends TOptionDouble
sealed trait TLong extends TOptionLong with TDouble with TNonOption

sealed trait TOptionFloat extends TOptionDouble
sealed trait TFloat extends TOptionFloat with TDouble with TNonOption

sealed trait TOptionInt extends TOptionLong with TOptionFloat
sealed trait TInt extends TOptionInt with TLong with TNonOption with TFloat

sealed trait TOptionByte extends TOptionInt
sealed trait TByte extends TOptionByte with TInt with TNonOption

sealed trait TOption 
 extends TOptionByte with TOptionInt with TOptionFloat with TOptionLong with TOptionDouble with TOptionBigDecimal
 with TOptionDate with TOptionString with TOptionTimestamp

sealed trait TNumericLowerTypeBound 
  extends TByte with TInt with TFloat with TLong with TDouble with TBigDecimal
 
sealed trait TNonOption

sealed trait TOptionLowerBound
 extends TOptionByte with TOptionInt with TOptionFloat with TOptionLong with TOptionDouble with TOptionBigDecimal
 with TOptionDate with TOptionString with TOptionTimestamp
 
sealed trait TEnumValue[A]
sealed trait TOptionEnumValue[A] extends TEnumValue[A]

sealed trait TString extends TOptionString with TNonOption
sealed trait TDate extends TOptionDate with TNonOption
sealed trait TTimestamp extends TOptionTimestamp with TNonOption
sealed trait TByteArray extends TOptionByteArray  with TNonOption
sealed trait TOptionString 
sealed trait TOptionDate
sealed trait TOptionTimestamp
sealed trait TOptionByteArray  
sealed trait TBoolean extends TOptionBoolean  with TNonOption
sealed trait TOptionBoolean
sealed trait TUUID extends TOptionUUID  with TNonOption
sealed trait TOptionUUID

@scala.annotation.implicitNotFound("The left side of the comparison (===, <>, between, ...) is not compatible with the right side.")
sealed class CanCompare[-A1,-A2]


trait TypedExpression[A1,T1] extends ExpressionNode {
  outer =>
    
  def plus[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = f.convert(new BinaryOperatorNode(this, e, "+"))

  def times[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = f.convert(new BinaryOperatorNode(this, e, "*"))

  def minus[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = f.convert(new BinaryOperatorNode(this, e, "-"))

  def div[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3, A4, T4]
         (e: TypedExpression[A2,T2])
         (implicit f:  TypedExpressionFactory[A3,T3], 
                   tf: Floatifier[T3,A4,T4]): TypedExpression[A4,T4] = tf.floatify(new BinaryOperatorNode(this, e, "/"))

  def +[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = plus(e)

  def *[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = times(e)

  def -[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3]
         (e: TypedExpression[A2,T2])
         (implicit f: TypedExpressionFactory[A3,T3]) : TypedExpression[A3,T3] = minus(e)

  def /[T3 >: T1 <: TNumeric, T2 <: T3, A2, A3, A4, T4]
         (e: TypedExpression[A2,T2])
         (implicit f:  TypedExpressionFactory[A3,T3], 
                   tf: Floatifier[T3,A4,T4]): TypedExpression[A4,T4] = tf.floatify(new BinaryOperatorNode(this, e, "/"))

  def ===[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new EqualityExpression(this, b)
  def <>[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<>")
  
  def gt[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">")
  def lt[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<")
  def gte[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new BinaryOperatorNodeLogicalBoolean(this, b, ">=")
  def lte[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = new BinaryOperatorNodeLogicalBoolean(this, b, "<=")
  
  def >[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = gt(b)
  def <[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = lt(b)
  def >=[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = gte(b)
  def <=[A2,T2](b: TypedExpression[A2,T2])(implicit ev: CanCompare[T1, T2]) = lte(b)
  
  //TODO: add T1 <:< TOption to isNull and isNotNull 
  def isNull= new PostfixOperatorNode("is null", this) with LogicalBoolean
  def isNotNull= new PostfixOperatorNode("is not null", this) with LogicalBoolean
  
  def between[A2,T2,A3,T3](b1: TypedExpression[A2,T2], 
                           b2: TypedExpression[A3,T3])
                          (implicit ev1: CanCompare[T1, T2], 
                                    ev2: CanCompare[T2, T3]) = new BetweenExpression(this, b1, b2)
  
  def like[A2,T2 <: TOptionString](s: TypedExpression[A2,T2])(implicit ev: CanCompare[T1,T2]) = new BinaryOperatorNodeLogicalBoolean(this, s, "like")
  
  def ||[A2,T2](e: TypedExpression[A2,T2]) = new ConcatOp[A1,A2,T1,T2](this, e)
    
  def regex(pattern: String) = new FunctionNode(pattern, Seq(this)) with LogicalBoolean {

    override def doWrite(sw: StatementWriter) =
      Session.currentSession.databaseAdapter.writeRegexExpression(outer, pattern, sw)
  }
  
  def is(columnAttributes: AttributeValidOnNumericalColumn*)(implicit restrictUsageWithinSchema: Schema) =
    new ColumnAttributeAssignment(_fieldMetaData, columnAttributes)
  
  
  def in[A2,T2](t: Traversable[A2])(implicit cc: CanCompare[T1,T2]): LogicalBoolean =  
    new InclusionOperator(this, new RightHandSideOfIn(new ConstantExpressionNodeList(t)).toIn)  
  
  def in[A2,T2](q: Query[A2])(implicit cc: CanCompare[T1,T2]): LogicalBoolean =
    new InclusionOperator(this, new RightHandSideOfIn(q.copy(false, Nil).ast))
  
  def notIn[A2,T2](t: Traversable[A2])(implicit cc: CanCompare[T1,T2]): LogicalBoolean =  
    new ExclusionOperator(this, new RightHandSideOfIn(new ConstantExpressionNodeList(t)).toNotIn)
  
  def notIn[A2,T2](q: Query[A2])(implicit cc: CanCompare[T1,T2]): LogicalBoolean =
    new ExclusionOperator(this, new RightHandSideOfIn(q.copy(false, Nil).ast))
  
  def ~ = this

  def sample:A1 = mapper.sample

  def mapper: OutMapper[A1]

  def :=[B <% TypedExpression[A1,T1]] (b: B) =     
    new UpdateAssignment(_fieldMetaData, b : TypedExpression[A1,T1])  

  def :=(q: Query[Measures[A1]]) =
    new UpdateAssignment(_fieldMetaData, q.ast)

  def defaultsTo[B <% TypedExpression[A1,T1]](b: B) /*(implicit restrictUsageWithinSchema: Schema) */ =
    new DefaultValueAssignment(_fieldMetaData, b : TypedExpression[A1,T1])

  /**
   * TODO: make safer with compiler plugin
   * Not type safe ! a TypedExpressionNode[T] might not be a SelectElementReference[_] that refers to a FieldSelectElement...   
   */
  private [squeryl] def _fieldMetaData = {
    val ser =
      try {
        this.asInstanceOf[SelectElementReference[_,_]]
      }
      catch { // TODO: validate this at compile time with a scalac plugin
        case e:ClassCastException => {
            throw new RuntimeException("left side of assignment '" + Utils.failSafeString(this.toString)+ "' is invalid, make sure statement uses *only* closure argument.", e)
        }
      }

    val fmd =
      try {
        ser.selectElement.asInstanceOf[FieldSelectElement].fieldMetaData
      }
      catch { // TODO: validate this at compile time with a scalac plugin
        case e:ClassCastException => {
          throw new RuntimeException("left side of assignment '" + Utils.failSafeString(this.toString)+ "' is invalid, make sure statement uses *only* closure argument.", e)
        }
      }
    fmd
  }
}


class TypedExpressionConversion[A1,T1](val e: ExpressionNode, bf: TypedExpressionFactory[A1,T1]) extends TypedExpression[A1,T1] {
  
  def mapper: OutMapper[A1] = bf.createOutMapper
  
  override def inhibited = e.inhibited

  override def doWrite(sw: StatementWriter) = e.doWrite((sw))

  override def children = e.children  
}

trait Floatifier[T1,A2,T2] {
  def floatify(v: ExpressionNode): TypedExpressionConversion[A2,T2]
}

trait IdentityFloatifier[A1,T1] extends Floatifier[T1,A1,T1]

trait FloatTypedExpressionFactory[A1,T1] extends TypedExpressionFactory[A1,T1] with IdentityFloatifier[A1,T1] {
  self: JdbcMapper[_,A1] =>
  def floatify(v: ExpressionNode): TypedExpressionConversion[A1,T1] = convert(v)
}


trait JdbcMapper[P,A] {
  self: TypedExpressionFactory[A,_] =>
  def thisTypedExpressionFactory: TypedExpressionFactory[A,_] = this
  def extractNativeJdbcValue(rs: ResultSet, i: Int): P
  def convertFromJdbc(v: P): A
  def convertToJdbc(v: A): P
  def defaultColumnLength: Int
  def map(rs: ResultSet, i: Int): A = convertFromJdbc(extractNativeJdbcValue(rs, i)) 
}


trait PrimitiveJdbcMapper[A] extends JdbcMapper[A,A] {
  self: TypedExpressionFactory[A,_] =>
  def extractNativeJdbcValue(rs: ResultSet, i: Int): A
  def convertFromJdbc(v: A) = v
  def convertToJdbc(v: A) = v
  def nativeJdbcType = sample.getClass
}

abstract class NonPrimitiveJdbcMapper[P,A,T](val primitiveMapper: PrimitiveJdbcMapper[P], val fieldMapper: FieldMapper) extends JdbcMapper[P,A] with TypedExpressionFactory[A,T] {
  self: TypedExpressionFactory[A,T] =>    
    
  def extractNativeJdbcValue(rs: ResultSet, i: Int): P = primitiveMapper.extractNativeJdbcValue(rs, i)
  def defaultColumnLength: Int = primitiveMapper.defaultColumnLength
  def sample: A = 
    convertFromJdbc(primitiveMapper.thisTypedExpressionFactory.sample)

 def createFromNativeJdbcValue(v: P) = create(convertFromJdbc(v))
 
  fieldMapper.register(this)  
}

trait TypedExpressionFactory[A,T] {
  self: JdbcMapper[_,A] =>
     
  def thisAnyRefMapper = this.asInstanceOf[JdbcMapper[AnyRef,A]]
  
  def create(a: A) : TypedExpression[A,T] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case None =>
        new ConstantTypedExpression[A,T](a, createOutMapper, thisAnyRefMapper.convertToJdbc(a))
      case Some(n:SelectElement) =>
        new SelectElementReference[A,T](n, createOutMapper)
    }
  /**
   * Converts the argument into a TypedExpression[A,T], the resulting expression
   * is meant to be equivalent in terms of SQL generation, the conversion is only
   * at the type level
   */
  def convert(v: ExpressionNode) = new TypedExpressionConversion[A,T](v,this)
  
  def sample: A
  
  def defaultColumnLength: Int
  
  def thisMapper: JdbcMapper[_,A] = this
  
  private def zis = this
  
  def createOutMapper: OutMapper[A] = new OutMapper[A] {
    
    def doMap(rs: ResultSet): A =       
      zis.map(rs, index)

    def sample:A = zis.sample
  }  
}

trait IntegralTypedExpressionFactory[A1,T1,A2,T2] 
  extends TypedExpressionFactory[A1,T1] with Floatifier[T1,A2,T2] {
  self: JdbcMapper[_,A1] =>
  
  def floatify(v: ExpressionNode): TypedExpressionConversion[A2,T2] = floatifyer.convert(v)
  def floatifyer: TypedExpressionFactory[A2,T2]
}


trait DeOptionizer[A1,T1,A2 <: Option[A1],T2] extends JdbcMapper[A1,A2] {
  self: TypedExpressionFactory[A2,T2] =>
    
  def deOptionizer: TypedExpressionFactory[A1,T1]
  
  def sample = Option(deOptionizer.sample)
  
  def defaultColumnLength: Int = deOptionizer.defaultColumnLength
    
  def convertFromJdbc(v: A1): A2 = Option(v).asInstanceOf[A2]
  /**
   * Jdbc uses nulls, we work with A1 <: Any, so we must hide this
   * to the compiler by allowing a null returning function pose as a Function1[A2,A1]
   */
  private val imposter = { 
    (a2:A2) => if(a2 == None) null else (a2:Option[A1]).get.asInstanceOf[AnyRef]
  }.asInstanceOf[Function1[A2,A1]]
  
  def convertToJdbc(v: A2) = imposter(v)
  
  def extractNativeJdbcValue(rs: ResultSet, i: Int) = deOptionizer.thisMapper.map(rs,i) 
  
  override def createOutMapper: OutMapper[A2] = new OutMapper[A2] {
    def doMap(rs: ResultSet): A2 = {
          
      val v = deOptionizer.thisMapper.map(rs,index)
      val r = 
        if(rs.wasNull)
          None
        else
          Option(v)
          
      r.asInstanceOf[A2]
    }
    
    def sample:A2 = convertFromJdbc(deOptionizer.sample)
  }
}

class ConcatOp[A1,A2,T1,T2](val a1: TypedExpression[A1,T1], val a2: TypedExpression[A2,T2]) extends BinaryOperatorNode(a1,a2, "||") {
  override def doWrite(sw: StatementWriter) =
      sw.databaseAdapter.writeConcatOperator(a1, a2, sw)   
}


class NvlNode[A,T](e1: TypedExpression[_,_], e2: TypedExpression[A,T]) 
  extends BinaryOperatorNode(e1,e2,"nvl", false) with TypedExpression[A,T] {

   def mapper = e2.mapper

   override def doWrite(sw: StatementWriter) =
    sw.databaseAdapter.writeNvlCall(left, right, sw)         
}
