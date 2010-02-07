package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.{QueryYield}
import org.squeryl.dsl.ast.OrderByArg
import org.squeryl.dsl.fsm.BaseQueryYield

trait OrderBySignatures[R] {
  self: BaseQueryYield[R] =>

  type O = OrderByArg

  def orderBy(e1: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O, e3: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O, e6: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _, e6 _)
    this
  }

  def orderBy(e1: =>O, e2: =>O, e3: =>O, e4: =>O, e5: =>O, e6: =>O, e7: =>O): QueryYield[R] = {
    _orderByExpressions = ()=> List(e1 _, e2 _, e3 _, e4 _, e5 _, e6 _, e7 _)
    this
  }
}