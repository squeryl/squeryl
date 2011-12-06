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
package org.squeryl.dsl.fsm
/*
import org.squeryl.dsl.{BinaryAMSOp, NonNumericalCoalesce, NonNumericalExpression, NumericalExpression}
import org.squeryl.dsl.ast.TypedExpressionNode

class CaseOfNonNumericalExpressionMatchStart[A](toMatch: NonNumericalExpression[A]) {

  def when[B,C,D](m: NonNumericalExpression[B], r: NumericalExpression[D])
                 (implicit ev1: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) =
    new CaseOfNonNumericalExpressionMatchYieldingNumerical[C,D](m.asInstanceOf[NonNumericalExpression[C]], r, None, Some(toMatch))

  def when[B,C,D](m: NonNumericalExpression[B], r: NonNumericalExpression[D])
                 (implicit ev1: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) =
    new CaseOfNonNumericalExpressionMatchYieldingNonNumerical[C,D](m.asInstanceOf[NonNumericalExpression[C]], r, None, Some(toMatch))
}


class CaseOfNonNumericalExpressionMatchYieldingNumerical[A,T](
  val whenArg: TypedExpressionNode[A],
  val thenArg: NumericalExpression[T],
  val previous: Option[CaseOfChain],
  val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOfChain {

  def when[B,C,U,V](m: NonNumericalExpression[B], r: NumericalExpression[U])
                   (implicit ev1: NonNumericalCoalesce[A,B] => NonNumericalExpression[C],
                             ev2: BinaryAMSOp[T,U] => NumericalExpression[V]) =
    new CaseOfNonNumericalExpressionMatchYieldingNumerical[C,V](m.asInstanceOf[NonNumericalExpression[C]], r.asInstanceOf[NumericalExpression[V]], Some(this))

  def otherwise[U,V](r: NumericalExpression[U])
                    (implicit ev1: BinaryAMSOp[T,U] => NumericalExpression[V]) = {

    val o = new BinaryAMSOp(thenArg, r, "!CaseOfNonNumericalExpressionMatchYieldingNumerical") : NumericalExpression[V]

    new CaseOfChainNumericalTermination[V](o.mapper, r, this)
  }
}


class CaseOfNonNumericalExpressionMatchYieldingNonNumerical[A,T](
  val whenArg: TypedExpressionNode[A],
  val thenArg: NonNumericalExpression[T],
  val previous: Option[CaseOfChain],
  val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOfChain {

  def when[B,C,U,V](m: NonNumericalExpression[B], r: NonNumericalExpression[U])
                   (implicit ev1: NonNumericalCoalesce[A,B] => NonNumericalExpression[C],
                             ev2: NonNumericalCoalesce[T,U] => NonNumericalExpression[V]) =
    new CaseOfNonNumericalExpressionMatchYieldingNonNumerical[C,V](m.asInstanceOf[NonNumericalExpression[C]], r.asInstanceOf[NonNumericalExpression[V]], Some(this), None)

  def otherwise[U,V](r: NonNumericalExpression[U])
                    (implicit ev2: NonNumericalCoalesce[T,U] => NonNumericalExpression[V]) = {

    val c = new NonNumericalCoalesce[T,U](thenArg, r, "!CaseOfNonNumericalExpressionMatchYieldingNonNumerical") : NonNumericalExpression[V]

    new CaseOfChainNonNumericalTermination[V](c.mapper, r, this)
  }
}

*/