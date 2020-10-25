object FromSignatures {

  private[this] def T(n: Int) = (1 to n).map("T" + _).mkString(", ")
  private[this] def t(n: Int) = (1 to n).map("t" + _).mkString(", ")

  private[this] def fromMethod(n: Int): String = s"""
  def from[${T(n)}, R](
    ${(1 to n).map(x => s"t${x}: Queryable[T${x}]").mkString(", ")}
  )(
    f: Function${n}[${T(n)}, QueryYield[R]]
  ): Query[R] =
    new Query${n}[${T(n)}, R](${t(n)}, f, true, Nil)"""

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.QueryYield
import org.squeryl.{Queryable, Query}

trait FromSignatures {
${(1 to size).map(fromMethod).mkString("\n")}
}
"""

}
