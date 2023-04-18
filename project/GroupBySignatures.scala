object GroupBySignatures {

  private[this] def method(n: Int): String = {
    def f(x: Int => String) = (1 to n).map(x).mkString(", ")
    val T = f("T" + _)

    s"""
  def groupBy[${T}](
    ${f(x => s"e${x}: => TypedExpression[T${x}, _]")}
  ): GroupByState[Product${n}[${T}]] =
    new GroupQueryYield[Product${n}[${T}]](
      this,
      () => List(${f("e" + _)})
    )"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.fsm.{GroupQueryYield, QueryElements, GroupByState}
import org.squeryl.dsl.TypedExpression

trait GroupBySignatures {
  self: QueryElements[_] =>

  def groupBy[T1](e1: => TypedExpression[T1, _]): GroupByState[T1] =
    new GroupQueryYield[T1](this, () => List(e1))

${(2 to size).map(method).mkString("\n")}
}
"""

}
