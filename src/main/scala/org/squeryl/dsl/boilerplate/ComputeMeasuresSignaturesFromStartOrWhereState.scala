package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.ast.ComputeArg
import org.squeryl.dsl.fsm.{MeasuresQueryYield, QueryElements, ComputeStateStartOrWhereState}

trait ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements =>

  def compute[T1](e1: =>ComputeArg[T1]): ComputeStateStartOrWhereState[T1] =
    new MeasuresQueryYield[T1](
      this,
      () =>List(e1)
    )

  def compute[T1,T2](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2]): ComputeStateStartOrWhereState[(T1,T2)] =
    new MeasuresQueryYield[(T1,T2)](
      this,
      () =>List(e1, e2)
    )

  def compute[T1,T2,T3](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3]): ComputeStateStartOrWhereState[(T1,T2,T3)] =
    new MeasuresQueryYield[(T1,T2,T3)](
      this,
      () =>List(e1, e2, e3)
    )

  def compute[T1,T2,T3,T4]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4]): ComputeStateStartOrWhereState[(T1,T2,T3,T4)] =
    new MeasuresQueryYield[(T1,T2,T3,T4)](
      this,
      () =>List(e1, e2, e3, e4)
    )

  def compute[T1,T2,T3,T4,T5]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5)](
      this,
      () =>List(e1, e2, e3, e4, e5)
    )

  def compute[T1,T2,T3,T4,T5,T6]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5,T6)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5,T6)](
      this,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6], e7: =>ComputeArg[T7]): ComputeStateStartOrWhereState[(T1,T2,T3,T4,T5,T6,T7)] =
    new MeasuresQueryYield[(T1,T2,T3,T4,T5,T6,T7)](
      this,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}