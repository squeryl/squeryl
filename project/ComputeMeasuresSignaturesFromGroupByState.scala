object ComputeMeasuresSignaturesFromGroupByState {

  private[this] def method(n: Int): String = {
    val T = (1 to n).map("T" + _).mkString(", ")

    s"""
  def compute[${T}](
    ${(1 to n).map(x => s"e${x}: => TypedExpression[T${x}, _]").mkString(", ")}
  ): ComputeStateFromGroupByState[G, Product${n}[${T}]] =
    new GroupWithMeasuresQueryYield[G, Product${n}[${T}]](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () => List(${(1 to n).map("e" + _).mkString(", ")})
    )
"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.fsm._
import org.squeryl.dsl.TypedExpression

trait ComputeMeasuresSignaturesFromGroupByState[G] {
  self: GroupQueryYield[G] =>

  def compute[T1](e1: => TypedExpression[T1, _]): ComputeStateFromGroupByState[G, T1] =
    new GroupWithMeasuresQueryYield[G, T1](
      this.queryElementzz,
      this.groupByClauseClosure,
      this.unevaluatedHavingClause,
      () => List(e1)
    )

${(2 to size).map(method).mkString("\n")}
}
"""

}
