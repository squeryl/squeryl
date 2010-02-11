package org.squeryl

import dsl.ast.{LogicalBoolean, TypedExpressionNode}
import dsl.{QueryDsl, Scalar}
import internals.ResultSetMapper
import java.sql.ResultSet

trait Queryable[T] {
  
  def name: String

  private[squeryl] var inhibited = false

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) : T

  def where(whereClauseFunctor: T => TypedExpressionNode[LogicalBoolean])(implicit dsl: QueryDsl): Query[T] = {
    import dsl._
    from(this)(q0 =>
      dsl.where(whereClauseFunctor(q0))
      select(q0)
    )
  }
}