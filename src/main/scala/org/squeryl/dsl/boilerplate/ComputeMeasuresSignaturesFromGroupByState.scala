package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.ast.GroupArg
import org.squeryl.dsl.fsm._

trait ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>

  def compute[T1](e1: =>GroupArg[T1]): ComputeStateFromGroupByState[G,T1] =
    new GroupWithMeasuresQueryYield[G,T1](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1)
    )

  def compute[T1,T2](e1: =>GroupArg[T1], e2: =>GroupArg[T2]): ComputeStateFromGroupByState[G,(T1,T2)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2)
    )

  def compute[T1,T2,T3](e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3]): ComputeStateFromGroupByState[G,(T1,T2,T3)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3)
    )

  def compute[T1,T2,T3,T4](e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4)
    )

  def compute[T1,T2,T3,T4,T5]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5)
    )

  def compute[T1,T2,T3,T4,T5,T6]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5], e6: =>GroupArg[T6]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6)
    )

  def compute[T1,T2,T3,T4,T5,T6,T7]
    (e1: =>GroupArg[T1], e2: =>GroupArg[T2], e3: =>GroupArg[T3], e4: =>GroupArg[T4],
     e5: =>GroupArg[T5], e6: =>GroupArg[T6], e7: =>GroupArg[T7]): ComputeStateFromGroupByState[G,(T1,T2,T3,T4,T5,T6,T7)] =
    new GroupWithMeasuresQueryYield[G,(T1,T2,T3,T4,T5,T6,T7)](
      this.queryElementzz,
      this.groupByClauseClosure,
      () =>List(e1, e2, e3, e4, e5, e6, e7)
    )
}