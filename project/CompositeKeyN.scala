object CompositeKeyN {

  private[this] def clazz(n: Int): String = {
    val A: String = (1 to n).map("A" + _).mkString(", ")
    def a(f: Int => String) = (1 to n).map(f).mkString(", ")
    val ck: String = a("ck._" + _)

    s"""
case class CompositeKey${n}[${A}](${a(x => s"a${x}: A${x}")})(implicit
  ${a(x => s"ev${x}: A${x} => TypedExpression[A${x}, _]")}
) extends CompositeKey {

  def ===(ck: CompositeKey${n}[${A}]) =
    buildEquality(ck)

  def ===(ck: (${A})) =
    buildEquality(CompositeKey${n}(${ck}))

  def in(cks: CompositeKey${n}[${A}]*) = inExpr(cks)
  def inTuples(cks: (${A})*) = inExpr(cks.map(ck => CompositeKey${n}(${ck})))

  def notIn(cks: CompositeKey${n}[${A}]*) = notInExpr(cks)
  def notInTuples(cks: (${A})*) = notInExpr(cks.map(ck => CompositeKey${n}(${ck})))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(${a(x => s"ev${x}(a${x})")})
}
"""
  }

  def value(size: Int) = s"""package org.squeryl.dsl

${(2 to size).map(clazz).mkString("\n")}
"""

}
