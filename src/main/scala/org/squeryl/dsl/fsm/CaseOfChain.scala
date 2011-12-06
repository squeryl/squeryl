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
import org.squeryl.dsl.ast.{TypedExpressionNode, ExpressionNode}
import org.squeryl.internals.{StatementWriter, OutMapper}
import collection.mutable.ArrayBuffer
import org.squeryl.dsl.{NonNumericalExpression, NumericalExpression}

trait CaseOfChain {

  def whenArg: ExpressionNode

  def thenArg: TypedExpressionNode[_]

  def previous: Option[CaseOfChain]

  def expressionToMatch: Option[TypedExpressionNode[_]]
}


class CaseOfChainTermination[A](val mapper: OutMapper[A], val otherwise : TypedExpressionNode[_], prev: CaseOfChain) extends ExpressionNode {

  def doWrite(sw: StatementWriter) = {

    val conds = new ArrayBuffer[ExpressionNode]
    val exprs = new ArrayBuffer[TypedExpressionNode[_]]

    var p: Option[CaseOfChain] = Some(prev)
    var toMatch: Option[ExpressionNode] = None

    while(p != None) {
      exprs.prepend(p.get.thenArg)
      conds.prepend(p.get.whenArg)

      val isLast = p.get.previous == None

      if(isLast)
        toMatch = p.get.expressionToMatch
      else
        assert(p.get.expressionToMatch == None)

      p = p.get.previous
    }

    assert(conds.size == exprs.size)

    val cases = conds.zip(exprs)

    sw.databaseAdapter.writeCaseStatement(toMatch, cases, otherwise, sw)
  }
}

class CaseOfChainNumericalTermination[A](m: OutMapper[A], o : TypedExpressionNode[_], p: CaseOfChain)
  extends CaseOfChainTermination[A](m, o, p) with NumericalExpression[A]

class CaseOfChainNonNumericalTermination[A](m: OutMapper[A], o : TypedExpressionNode[_], p: CaseOfChain)
  extends CaseOfChainTermination[A](m, o, p) with NonNumericalExpression[A]

*/