package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.ast.ComputeArg
import org.squeryl.dsl.{GroupWithMeasuresQueryYield, GroupQueryYield}
import org.squeryl.dsl.fsm._

trait ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>

  def Compute[T1](e1: =>ComputeArg[T1]): ComputeStateFromGroupByState[G,T1] =
    new GroupWithMeasuresQueryYield[G,T1](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1)
    )

  def Compute[T1,T2](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2]): ComputeStateFromGroupByState[G,(T1,T2)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2)
    )

  def Compute[T1,T2,T3](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3]): ComputeStateFromGroupByState[G,(T1,T2,T3)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3)
    )

  def Compute[T1,T2,T3,T4](e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4)
    )

  def Compute[T1,T2,T3,T4,T5]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5)
    )

  def Compute[T1,T2,T3,T4,T5,T6]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def Compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>ComputeArg[T1], e2: =>ComputeArg[T2], e3: =>ComputeArg[T3], e4: =>ComputeArg[T4],
     e5: =>ComputeArg[T5], e6: =>ComputeArg[T6], e7: =>ComputeArg[T7]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6,T7)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6,T7)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}