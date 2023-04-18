object OrderBySignatures {

  private[this] def method(n: Int): String = {
    def f(x: Int => String) = (1 to n).map(x).mkString(", ")

    s"""
  def orderBy(
    ${f(x => s"e${x}: => O")}
  ): QueryYield[R] = {
    _orderByExpressions = () => List(${f("() => e" + _)})
    this
  }"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.QueryYield
import org.squeryl.dsl.fsm.BaseQueryYield
import org.squeryl.dsl.ast.ExpressionNode

trait OrderBySignatures[R] {
  self: BaseQueryYield[R] =>

  type O = ExpressionNode

  def orderBy(args: List[O]): QueryYield[R] = {
    _orderByExpressions = () => args.map(() => _)
    this
  }

${(1 to size).map(method).mkString("\n")}
}
"""

}
