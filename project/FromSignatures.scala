object FromSignatures {

  private[this] def fromMethod(n: Int): String = {
    val T = (1 to n).map("T" + _).mkString(", ")
    val t = (1 to n).map("t" + _).mkString(", ")

    s"""
  def from[${T}, R](
    ${(1 to n).map(x => s"t${x}: Queryable[T${x}]").mkString(", ")}
  )(
    f: Function${n}[${T}, QueryYield[R]]
  ): Query[R] =
    new Query${n}[${T}, R](${t}, f, true, Nil)"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.QueryYield
import org.squeryl.{Queryable, Query}

trait FromSignatures {
${(1 to size).map(fromMethod).mkString("\n")}
}
"""

}
