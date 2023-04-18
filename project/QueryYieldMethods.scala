object QueryYieldMethods {

  private[this] def method(n: Int): String = {
    def a(f: Int => String) = (1 to n).map(f).mkString(", ")

    s"""
  def on(
    ${a(x => s"lb${x}: => LogicalBoolean")}
  ): JoinQueryYield${n}[R] = {
    this.joinExpressions = Seq(${a(x => s"() => lb${x}")})
    new JoinQueryYield${n}[R](this)
  }"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl

import org.squeryl.dsl.ast.LogicalBoolean
import org.squeryl.dsl.boilerplate._

trait QueryYieldMethods[R] { self: QueryYield[R] =>

${(1 to size).map(method).mkString("\n")}

}
"""

}
