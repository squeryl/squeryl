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
import collection.mutable.ArrayBuffer
import org.squeryl.Schema
import org.squeryl.internals.{AttributeValidOnMultipleColumn, ColumnAttribute, FieldMetaData}

trait CompositeKey {

  private [squeryl] var _members: Option[Seq[SelectElementReference[_,_]]] = None

  private [squeryl] var _propertyName: Option[String] = None

  private [squeryl] def _fields: Seq[FieldMetaData] =
    if(_members == None)
      List.empty
    else
      _members.get.map(_.selectElement.asInstanceOf[FieldSelectElement].fieldMetaData)

  protected def constantMembers: Iterable[TypedExpression[_,_]]

  protected def members: Iterable[TypedExpression[_,_]] =
    _members.getOrElse(constantMembers)

  private [squeryl] def buildEquality(ck: CompositeKey): LogicalBoolean = {
      
    val equalities = (members zip ck.members).map(t => new EqualityExpression(t._1, t._2))

    val head = equalities.head
    val tail = equalities.tail

    tail.foldLeft(equalities.head : LogicalBoolean)((a,b) => new BinaryOperatorNodeLogicalBoolean(a, b, "and"))
  }

  def is(attributes: AttributeValidOnMultipleColumn*) = new CompositeKeyAttributeAssignment(this, attributes)  
}

case class CompositeKey2[A1,A2](val a1:A1, val a2: A2) extends CompositeKey {

  def ===(ck: CompositeKey2[A1,A2]) =
    buildEquality(ck)

  def ===(ck: Tuple2[A1,A2]) =
    buildEquality(new CompositeKey2(ck._1, ck._2))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2)
  )

}

case class CompositeKey3[A1,A2,A3](val a1:A1, val a2: A2, val a3: A3) extends CompositeKey {

  def ===(ck: CompositeKey3[A1,A2,A3]) =
    buildEquality(ck)

  def ===(ck: Tuple3[A1,A2,A3]) =
    buildEquality(new CompositeKey3(ck._1, ck._2, ck._3))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3)
  )
}

case class CompositeKey4[A1,A2,A3,A4](val a1:A1, val a2: A2, val a3: A3, val a4: A4) extends CompositeKey {

  def ===(ck: CompositeKey4[A1,A2,A3,A4]) =
    buildEquality(ck)

  def ===(ck: Tuple4[A1,A2,A3,A4]) =
    buildEquality(new CompositeKey4(ck._1, ck._2, ck._3, ck._4))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4)
  )
}


case class CompositeKey5[A1,A2,A3,A4,A5](val a1:A1, val a2: A2, val a3: A3, val a4: A4, val a5: A5) extends CompositeKey {

  def ===(ck: CompositeKey5[A1,A2,A3,A4,A5]) =
    buildEquality(ck)

  def ===(ck: Tuple5[A1,A2,A3,A4,A5]) =
    buildEquality(new CompositeKey5(ck._1, ck._2, ck._3, ck._4, ck._5))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4),
    new InputOnlyConstantExpressionNode(a5)
  )
}
case class CompositeKey6[A1,A2,A3,A4,A5,A6](val a1:A1, val a2: A2, val a3: A3, val a4: A4, val a5: A5, val a6: A6) extends CompositeKey {

  def ===(ck: CompositeKey6[A1,A2,A3,A4,A5,A6]) =
    buildEquality(ck)

  def ===(ck: Tuple6[A1,A2,A3,A4,A5,A6]) =
    buildEquality(new CompositeKey6(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4),
    new InputOnlyConstantExpressionNode(a5),
    new InputOnlyConstantExpressionNode(a6)
  )
}

case class CompositeKey7[A1,A2,A3,A4,A5,A6,A7](val a1:A1, val a2: A2, val a3: A3, val a4: A4, val a5: A5, val a6: A6, val a7: A7) extends CompositeKey {

  def ===(ck: CompositeKey7[A1,A2,A3,A4,A5,A6,A7]) =
    buildEquality(ck)

  def ===(ck: Tuple7[A1,A2,A3,A4,A5,A6,A7]) =
    buildEquality(new CompositeKey7(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6,ck._7))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4),
    new InputOnlyConstantExpressionNode(a5),
    new InputOnlyConstantExpressionNode(a6),
    new InputOnlyConstantExpressionNode(a7)
  )
}

case class CompositeKey8[A1,A2,A3,A4,A5,A6,A7,A8](val a1:A1, val a2: A2, val a3: A3, val a4: A4, val a5: A5, val a6: A6, val a7: A7, val a8: A8) extends CompositeKey {

  def ===(ck: CompositeKey8[A1,A2,A3,A4,A5,A6,A7,A8]) =
    buildEquality(ck)

  def ===(ck: Tuple8[A1,A2,A3,A4,A5,A6,A7,A8]) =
    buildEquality(new CompositeKey8(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6,ck._7,ck._8))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4),
    new InputOnlyConstantExpressionNode(a5),
    new InputOnlyConstantExpressionNode(a6),
    new InputOnlyConstantExpressionNode(a7),
    new InputOnlyConstantExpressionNode(a8)
  )
}

case class CompositeKey9[A1,A2,A3,A4,A5,A6,A7,A8,A9](val a1:A1, val a2: A2, val a3: A3, val a4: A4, val a5: A5, val a6: A6, val a7: A7, val a8: A8,val a9: A9) extends CompositeKey {

  def ===(ck: CompositeKey9[A1,A2,A3,A4,A5,A6,A7,A8,A9]) =
    buildEquality(ck)

  def ===(ck: Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9]) =
    buildEquality(new CompositeKey9(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6,ck._7,ck._8,ck._9))

  protected def constantMembers: Iterable[TypedExpression[_,_]] = List(
    new InputOnlyConstantExpressionNode(a1),
    new InputOnlyConstantExpressionNode(a2),
    new InputOnlyConstantExpressionNode(a3),
    new InputOnlyConstantExpressionNode(a4),
    new InputOnlyConstantExpressionNode(a5),
    new InputOnlyConstantExpressionNode(a6),
    new InputOnlyConstantExpressionNode(a7),
    new InputOnlyConstantExpressionNode(a8),
    new InputOnlyConstantExpressionNode(a9)
  )
}
