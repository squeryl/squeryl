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

import org.squeryl.internals.{StatementWriter, OutMapper}
import collection.mutable.ArrayBuffer
import org.squeryl.dsl.ast.{TypedExpressionNode, ExpressionNode, LogicalBoolean}
import org.squeryl.dsl.{NonNumericalCoalesce, BinaryAMSOp, NonNumericalExpression, NumericalExpression}

class CaseOnConditionChainStart {

  def when[A](condition: LogicalBoolean, r: NumericalExpression[A]) = new CaseOnConditionChainNumerical[A](condition, r, None, None)

  def when[A](condition: LogicalBoolean, r: NonNumericalExpression[A]) = new CaseOnConditionChainNonNumerical[A](condition, r, None, None)
}

//class CaseOnExpressionToMatchStart[A] {
//
//  def when[B](m: NumericalExpression[A], r: NumericalExpression[B]) = new CaseOnConditionChainNumerical[A](condition, r, None, None)
//
//  def when[B](m: NonNumericalExpression[A], r: NonNumericalExpression[B]) = new CaseOnConditionChainNonNumerical[A](condition, r, None, None)
//}

class CaseOnConditionChainTermination[A](val mapper: OutMapper[A], val otherwise : TypedExpressionNode[_], prev: CaseOnConditionChain) extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {

    val conds = new ArrayBuffer[ExpressionNode]
    val exprs = new ArrayBuffer[TypedExpressionNode[_]]

    var p: Option[CaseOnConditionChain] = Some(prev)

    while(p != None) {
      exprs.prepend(p.get.thenArg)
      conds.prepend(p.get.whenArg)
      p = p.get.previous
    }

    assert(conds.size == exprs.size)

    val cases = conds.zip(exprs)

    sw.databaseAdapter.writeCaseStatement(None, cases, otherwise, sw)
  }
}

class CaseOnConditionChainNumericalTermination[A](m: OutMapper[A], o : TypedExpressionNode[_], p: CaseOnConditionChain)
  extends CaseOnConditionChainTermination[A](m, o, p) with NumericalExpression[A]

class CaseOnConditionChainNonNumericalTermination[A](m: OutMapper[A], o : TypedExpressionNode[_], p: CaseOnConditionChain)
  extends CaseOnConditionChainTermination[A](m, o, p) with NonNumericalExpression[A]


trait CaseOnConditionChain {

  def whenArg: ExpressionNode

  def thenArg: TypedExpressionNode[_]

  def previous: Option[CaseOnConditionChain]

  def expressionToMatch: Option[TypedExpressionNode[_]]
}

class CaseOnConditionChainNumerical[A](val whenArg: LogicalBoolean, val thenArg: NumericalExpression[_], val previous: Option[CaseOnConditionChainNumerical[_]], val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOnConditionChain {

  def when[B,C](condition: LogicalBoolean, r: NumericalExpression[B])(implicit ev: BinaryAMSOp[A,B] => NumericalExpression[C]) =
    new CaseOnConditionChainNumerical[C](condition, r, Some(this))

  def otherwise[B,C](r: NumericalExpression[B])(implicit ev: BinaryAMSOp[A,B] => NumericalExpression[C]) = {

    val bo = new BinaryAMSOp[A,B](thenArg.asInstanceOf[NumericalExpression[A]], r,"!CaseOnConditionChainNumerical!") : NumericalExpression[C]
    new CaseOnConditionChainNumericalTermination(bo.mapper, r, this)
  }

}

class CaseOnConditionChainNonNumerical[A](val whenArg: LogicalBoolean, val thenArg: NonNumericalExpression[_], val previous: Option[CaseOnConditionChainNonNumerical[_]], val expressionToMatch: Option[TypedExpressionNode[_]] = None)
  extends CaseOnConditionChain {

  def when[B,C](expr: LogicalBoolean, r: NonNumericalExpression[B])(implicit ev: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) =
    new CaseOnConditionChainNonNumerical[C](expr, r, Some(this))


  def otherwise[B,C](r: NonNumericalExpression[B])(implicit ev: NonNumericalCoalesce[A,B] => NonNumericalExpression[C]) = {

    val nnC = new NonNumericalCoalesce[A,B](thenArg.asInstanceOf[NonNumericalExpression[A]], r, "!CaseOnConditionChainNonNumerical") : NonNumericalExpression[C]
    new CaseOnConditionChainNonNumericalTermination(nnC.mapper, r, this)
  }
}
