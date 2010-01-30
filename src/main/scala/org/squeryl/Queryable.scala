package org.squeryl

import internals.ResultSetMapper
import java.sql.ResultSet

trait Queryable[+T] {
  
  def name: String
  
  private[squeryl] def give(resultSetMapper: ResultSetMapper, rs: ResultSet) : T
}