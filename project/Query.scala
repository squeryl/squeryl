object Query {

  private[this] def queryClass(n: Int): String = {
    val T: String = (1 to n).map("T" + _).mkString(", ")
    val t: String = (1 to n).map("t" + _).mkString(", ")

    s"""
class Query${n}[${T}, R](
  ${(1 to n).map(x => s"t${x}: Queryable[T${x}]").mkString(", ")},
  f: Function${n}[${T}, QueryYield[R]],
  isRoot: Boolean,
  unions: List[(String, Query[R])]
) extends AbstractQuery[R](isRoot, unions) {

${(1 to n).map(x => s"  val sq${x} = createSubQueryable(t${x})").mkString("\n")}

  def createCopy(root: Boolean, newUnions: List[(String, Query[R])]): AbstractQuery[R] =
    new Query${n}[${T}, R](${t}, f, root, copyUnions(unions ++ newUnions))

  def invokeYield(rsm: ResultSetMapper, rs: ResultSet): R =
    f(${(1 to n).map { x => s"sq${x}.give(rs)" }.mkString(", ")}).invokeYield(rsm, rs)

  val ast = buildAst(
    f(${(1 to n).map("sq" + _ + ".sample").mkString(", ")}),
    ${(1 to n).map("sq" + _).mkString(", ")}
  )
}"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl.boilerplate

import java.sql.ResultSet
import org.squeryl.internals.ResultSetMapper
import org.squeryl.dsl.AbstractQuery
import org.squeryl.dsl.QueryYield
import org.squeryl.Query
import org.squeryl.Queryable

${(1 to size).map(queryClass).mkString("\n")}
"""

}
