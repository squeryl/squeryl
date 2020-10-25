object STuple {

  private[this] def T(n: Int) = (1 to n).map("T" + _).mkString(", ")

  private[this] def clazz(n: Int): String = s"""
class STuple${n}[${T(n)}](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple${n - 1}[${T(n - 1)}](n, m)
    with Product${n}[${T(n)}] {
  def _${n} = _get[T${n}](${n})
}
"""

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import org.squeryl.internals.OutMapper
import org.squeryl.dsl.ast.SelectElement

${(2 to size).map(clazz).mkString("\n")}
"""

}
