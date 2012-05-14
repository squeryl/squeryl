package org.squeryl.dsl

import org.squeryl.Query
import org.squeryl.internals.ResultSetMapper
import java.sql.ResultSet

class DelegateQuery[M](val q: Query[M]) extends Query[M] {

  def iterator = q.iterator

  def distinct = q.distinct

  def forUpdate = q.forUpdate

  def dumpAst = q.dumpAst

  def page(offset:Int, length:Int) = q.page(offset, length)

  def statement: String = q.statement

  def ast = q.ast

  protected[squeryl] def invokeYield(rsm: ResultSetMapper, rs: ResultSet) =
    q.invokeYield(rsm, rs)

  override private[squeryl] def copy(
      asRoot: Boolean,
      newUnions: List[(String, Query[M])]
    ): Query[M] =
    q.copy(asRoot, newUnions)

  def name = q.name

  private[squeryl] def give(rsm: ResultSetMapper, rs: ResultSet) =
    q.invokeYield(rsm, rs)

  def union(q0: Query[M]): Query[M] = q.union(q0)

  def unionAll(q0: Query[M]): Query[M] = q.unionAll(q0)

  def intersect(q0: Query[M]): Query[M] = q.intersect(q0)

  def intersectAll(q0: Query[M]): Query[M] = q.intersectAll(q0)

  def except(q0: Query[M]): Query[M] = q.except(q0)

  def exceptAll(q0: Query[M]): Query[M] = q.exceptAll(q0)
}
