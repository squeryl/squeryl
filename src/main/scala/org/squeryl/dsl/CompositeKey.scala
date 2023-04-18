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
import org.squeryl.internals.{AttributeValidOnMultipleColumn, FieldMetaData}

trait CompositeKey {

  private[squeryl] var _members: Option[collection.Seq[SelectElementReference[_, _]]] = None

  private[squeryl] var _propertyName: Option[String] = None

  private[squeryl] def _fields: collection.Seq[FieldMetaData] =
    if (_members.isEmpty)
      List.empty
    else
      _members.get.map(_.selectElement.asInstanceOf[FieldSelectElement].fieldMetaData)

  protected def constantMembers: Iterable[TypedExpression[_, _]]

  protected def members: Iterable[TypedExpression[_, _]] =
    _members.getOrElse(constantMembers)

  private[squeryl] def buildEquality(ck: CompositeKey): LogicalBoolean = {
    val equalities = (members zip ck.members).map(t => new EqualityExpression(t._1, t._2))

    equalities.head
    val tail = equalities.tail

    tail.foldLeft(equalities.head: LogicalBoolean)(_ and _)
  }

  def is(attributes: AttributeValidOnMultipleColumn*) = new CompositeKeyAttributeAssignment(this, attributes)

  protected def inExpr(cks: Iterable[CompositeKey]): LogicalBoolean = {
    new InclusionOperator(
      new RowValueConstructorNode(members.toList),
      new RightHandSideOfIn(
        new ListExpressionNode(
          cks.toList map (ck => new RowValueConstructorNode(ck.members.toList))
        ),
        Some(true)
      )
    )
  }

  protected def notInExpr(cks: Iterable[CompositeKey]): LogicalBoolean = {
    new ExclusionOperator(
      new RowValueConstructorNode(members.toList),
      new RightHandSideOfIn(
        new ListExpressionNode(
          cks.toList map (ck => new RowValueConstructorNode(ck.members.toList))
        ),
        Some(false)
      )
    )
  }
}
