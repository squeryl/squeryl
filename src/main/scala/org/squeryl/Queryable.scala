package org.squeryl

import internals.ResultSetMapper
import java.sql.ResultSet

trait Queryable[+T] {
  
  def name: String

  private[squeryl] var inhibited = false

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) : T

  /**
   * This method will throw an exception, a bug in the implicit conversion resolution
   *mechanism prevents the implicit def from being applied
   * org.squeryl.dsl.QueryDsl
   *  implicit def view2KeyedEntityView[K,A <: KeyedEntity[K]](v: Queryable[A]): KeyedEntityView[K,A] =
   * new KeyedEntityView[K,A](v)
   *
   * when a method called lookup is implemented here, it works ....
   */
  def lookup(s: ResultSet): Option[T] = error("!!!!")
}