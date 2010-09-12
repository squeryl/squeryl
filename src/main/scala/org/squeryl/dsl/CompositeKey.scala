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
import collection.mutable.ArrayBuffer
import org.squeryl.Schema
import org.squeryl.internals.{AttributeValidOnMultipleColumn, ColumnAttribute, FieldMetaData}

trait CompositeKey {

  private [squeryl] var _members: Option[Seq[SelectElementReference[Any]]] = None

  private [squeryl] var _propertyName: Option[String] = None

  private [squeryl] def _fields: Seq[FieldMetaData] =
    if(_members == None)
      List.empty
    else
      _members.get.map(_.selectElement.asInstanceOf[FieldSelectElement].fieldMataData)

  protected def constantMembers: Iterable[ExpressionNode]

  protected def members: Iterable[ExpressionNode] =
    _members.getOrElse(constantMembers)

  private [squeryl] def buildEquality(ck: CompositeKey): LogicalBoolean = {

    val it1 = members.iterator
    val it2 = ck.members.iterator

    // the type system guaranties that it1 and it2 have the same length

    val equalities = new ArrayBuffer[LogicalBoolean]

    while(it1.hasNext) {
      val n1 = it1.next
      val n2 = it2.next
      val e = new BinaryOperatorNodeLogicalBoolean(n1, n2, "=")
      equalities.append(e)
    }

    val first = equalities.remove(1)

    val r = equalities.foldLeft(first)((b,a) => new BinaryOperatorNodeLogicalBoolean(a, b, "and"))

    r
  }

  def is(attributes: AttributeValidOnMultipleColumn*) = new CompositeKeyAttributeAssignment(this, attributes)  
}

case class CompositeKey2[A1,A2](val a1:A1, val a2: A2) extends CompositeKey {

  def ===(ck: CompositeKey2[A1,A2]) =
    buildEquality(ck)

  def ===(ck: Tuple2[A1,A2]) =
    buildEquality(new CompositeKey2(ck._1, ck._2))

  protected def constantMembers: Iterable[ExpressionNode] = List(
    new ConstantExpressionNode[A1](a1),
    new ConstantExpressionNode[A2](a2)
  )

}

case class CompositeKey3[A1,A2,A3](val a1:A1, val a2: A2, val a3: A3) extends CompositeKey {

  def ===(ck: CompositeKey3[A1,A2,A3]) =
    buildEquality(ck)

  def ===(ck: Tuple3[A1,A2,A3]) =
    buildEquality(new CompositeKey3(ck._1, ck._2, ck._3))

  protected def constantMembers: Iterable[ExpressionNode] = List(
    new ConstantExpressionNode[A1](a1),
    new ConstantExpressionNode[A2](a2),
    new ConstantExpressionNode[A3](a3)
  )
}

case class CompositeKey4[A1,A2,A3,A4](val a1:A1, val a2: A2, val a3: A3, val a4: A4) extends CompositeKey {

  def ===(ck: CompositeKey4[A1,A2,A3,A4]) =
    buildEquality(ck)

  def ===(ck: Tuple4[A1,A2,A3,A4]) =
    buildEquality(new CompositeKey4(ck._1, ck._2, ck._3, ck._4))

  protected def constantMembers: Iterable[ExpressionNode] = List(
    new ConstantExpressionNode[A1](a1),
    new ConstantExpressionNode[A2](a2),
    new ConstantExpressionNode[A3](a3),
    new ConstantExpressionNode[A4](a4)
  )
}