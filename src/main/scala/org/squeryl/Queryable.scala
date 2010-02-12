package org.squeryl

import dsl.ast.{LogicalBoolean}
import dsl.{QueryDsl}
import internals.ResultSetMapper
import java.sql.ResultSet

trait Queryable[T] {
  
  def name: String

  private[squeryl] var inhibited = false

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) : T

  def where(whereClauseFunctor: T => LogicalBoolean)(implicit dsl: QueryDsl): Query[T] = {
    import dsl._
    from(this)(q0 =>
      dsl.where(whereClauseFunctor(q0))
      select(q0)
    )
  }
}