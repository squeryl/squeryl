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

import ast.{LogicalBoolean, ExpressionNode, QueryExpressionNode, SelectElement}
import boilerplate._
import org.squeryl.internals.{ResultSetMapper}
import java.sql.ResultSet

trait QueryYield[R] {

  def invokeYield(resultSetMapper: ResultSetMapper, rs: ResultSet): R

  def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper):
    (Iterable[SelectElement],AnyRef)

  def queryElements:
    (Option[ExpressionNode],
     Option[ExpressionNode],
     Iterable[ExpressionNode],
     Iterable[ExpressionNode])

  private [squeryl] var joinExpressions: Seq[()=>LogicalBoolean] = Nil

  def on(lb1: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _)
    new JoinQueryYield1(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _)
    new JoinQueryYield2(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean, lb3: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _, lb3 _)
    new JoinQueryYield3(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean, lb3: =>LogicalBoolean, lb4: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _, lb3 _, lb4 _)
    new JoinQueryYield4(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean, lb3: =>LogicalBoolean, lb4: =>LogicalBoolean, lb5: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _, lb3 _, lb4 _, lb5 _)
    new JoinQueryYield5(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean, lb3: =>LogicalBoolean, lb4: =>LogicalBoolean, lb5: =>LogicalBoolean, lb6: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _, lb3 _, lb4 _, lb5 _, lb6 _)
    new JoinQueryYield6(this)
  }

  def on(lb1: =>LogicalBoolean, lb2: =>LogicalBoolean, lb3: =>LogicalBoolean, lb4: =>LogicalBoolean, lb5: =>LogicalBoolean, lb6: =>LogicalBoolean, lb7: =>LogicalBoolean) = {
    joinExpressions = Seq(lb1 _, lb2 _, lb3 _, lb4 _, lb5 _, lb6 _, lb7 _)
    new JoinQueryYield7(this)
  }
}
