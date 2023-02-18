object ComputeMeasuresSignaturesFromStartOrWhereState {

  private[this] def fromMethod(n: Int): String = {
    def f(x: Int => String) = (1 to n).map(x).mkString(", ")
    val T = f("T" + _)

    s"""
  def compute[${T}](
    ${f(x => s"e${x}: => TypedExpression[T${x}, _]")}
  ): ComputeStateStartOrWhereState[Product${n}[${T}]] =
    new MeasuresQueryYield[Product${n}[${T}]](
      this,
      () => List(${f("e" + _)})
    )"""
  }

  def value(size: Int): String = {
    s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.fsm.{MeasuresQueryYield, QueryElements, ComputeStateStartOrWhereState}
import org.squeryl.dsl.TypedExpression

trait ComputeMeasuresSignaturesFromStartOrWhereState {
  self: QueryElements[_] =>

  def compute[T1](e1: => TypedExpression[T1, _]): ComputeStateStartOrWhereState[T1] =
    new MeasuresQueryYield[T1](
      this,
      () => List(e1)
    )

${(2 to size).map(fromMethod).mkString("\n")}
}
"""
  }

}
