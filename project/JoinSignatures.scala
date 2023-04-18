object JoinSignatures {
  private[this] def f(n: Int, s: Int => String) = {
    if (n == 1) {
      ""
    } else {
      (1 until n).map(s).mkString(", ", ", ", "")
    }
  }
  private[this] def B(n: Int) = f(n, "B" + _)
  private[this] def bB(n: Int) = f(n, x => s"b${x}: B${x}")
  private[this] def b(n: Int) = f(n, "b" + _)
  private[this] def q(n: Int) = f(n, "q" + _)
  private[this] def joinedQueryable(n: Int) = {
    if (n == 1) {
      ""
    } else {
      (1 until n).map(x => s"q$x: JoinedQueryable[B$x]").mkString(", ", ", ", "")
    }
  }

  private[this] def joinMethod(n: Int): String = {
    s"""
  def join[A${B(n)}, C](
    q: Queryable[A]${joinedQueryable(n)}
  )(
    f: Function${n}[A${B(n)}, JoinQueryYield${n - 1}[C]]
  ): Query[C] =
    from(q${q(n)})(
     (a: A${bB(n)}) => f(a${b(n)}).queryYield
    )
"""
  }

  def value(size: Int) = s"""
package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.{QueryYield}
import org.squeryl.dsl.internal.{JoinedQueryable, InnerJoinedQueryable, OuterJoinedQueryable}
import org.squeryl.{Queryable, Query}

trait JoinSignatures {
  self: FromSignatures =>

  class JoinPrecursor[A](q: Queryable[A]) {
    def leftOuter = new OuterJoinedQueryable[A](q, "left")
    def rightOuter = new OuterJoinedQueryable[A](q, "right")
    def fullOuter = new OuterJoinedQueryable[A](q, "full")
  }

  implicit def queryable2JoinPrecursor[A](q: Queryable[A]): JoinPrecursor[A] = new JoinPrecursor[A](q)

  implicit def queryable2RightInnerJoinedQueryable[A](q: Queryable[A]): InnerJoinedQueryable[A] =
    new InnerJoinedQueryable[A](q, "")

${(2 until size).map(joinMethod).mkString}
}

${(1 to size).map(n => s"class JoinQueryYield$n[R](val queryYield: QueryYield[R])").mkString("\n")}
"""

}
