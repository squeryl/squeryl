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
import org.squeryl.internals.{StatementWriter, OutMapper}
import collection.mutable.ArrayBuffer
import org.squeryl.dsl.ast.{TypedExpressionNode, ExpressionNode, LogicalBoolean}
import org.squeryl.dsl._

class CaseOfConditionChainStart {

  def when[A](condition: LogicalBoolean, r: NumericalExpression[A]) = new CaseOfConditionChainNumerical[A](condition, r, None, None)

  def when[A](condition: LogicalBoolean, r: NonNumericalExpression[A]) = new CaseOfConditionChainNonNumerical[A](condition, r, None, None)
}

class CaseOfConditionChainNumerical[A](val whenArg: LogicalBoolean, val thenArg: NumericalExpression[_], val previous: Option[CaseOfChain], val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOfChain {

  def when[B,C](condition: LogicalBoolean, r: NumericalExpression[B])(implicit ev: BinaryAMSOp[A,B] => NumericalExpression[C]) =
    new CaseOfConditionChainNumerical[C](condition, r, Some(this))

  def otherwise[B,C](r: NumericalExpression[B])(implicit ev: BinaryAMSOp[A,B] => NumericalExpression[C]) = {

    val bo = new BinaryAMSOp[A,B](thenArg.asInstanceOf[NumericalExpression[A]], r,"!CaseOfConditionChainNumerical!") : NumericalExpression[C]
    new CaseOfChainNumericalTermination(bo.mapper, r, this)
  }

}

class CaseOfConditionChainNonNumerical[A](val whenArg: LogicalBoolean, val thenArg: NonNumericalExpression[_], val previous: Option[CaseOfChain], val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOfChain {

  def when[B,C](expr: LogicalBoolean, r: NonNumericalExpression[B])(implicit ev: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) =
    new CaseOfConditionChainNonNumerical[C](expr, r, Some(this))


  def otherwise[B,C](r: NonNumericalExpression[B])(implicit ev: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) = {

    val nnC = new NonNumericalCoalesce[A,B](thenArg.asInstanceOf[NonNumericalExpression[A]], r, "!CaseOfConditionChainNonNumerical") : NonNumericalExpression[C]
    new CaseOfChainNonNumericalTermination(nnC.mapper, r, this)
  }
}

*/