package org.squeryl.dsl

import org.squeryl.Queryable
import org.squeryl.internals.ResultSetMapper
import java.sql.ResultSet


class OptionalQueryable[A](val queryable: Queryable[A]) extends Queryable[Option[A]] {


  def name = queryable.name

  def inhibitWhen(b: Boolean) = {
    inhibited = b
    this
  }

  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet)  =
    if(inhibited)
      None
    else
      Some(queryable.give(resultSetMapper, rs))
}