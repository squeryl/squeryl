package org.squeryl

import dsl.ast.{ExpressionNode}
import internals.ResultSetMapper
import java.sql.ResultSet

trait Query[R] extends Iterable[R] with Queryable[R] {

  protected[squeryl] def invokeYield(rsm: ResultSetMapper, resultSet: ResultSet): R

  def dumpAst: String

  /**
   * returns a 'pretty' statement, i.e. values are printed instead of '?'  
   */
  def statement: String

  def ast: ExpressionNode

  private[squeryl] def copy(asRoot:Boolean): Query[R]

  /**
   * Returns the first row of the query. An exception will be thrown
   * if the query returns no row or more than one row.
   */
  def single: R = {
    val i = iterator
    val r = i.next
    if(i.hasNext)
      error("single called on query returning more than one row : \n" + statement)
    r
  }


  def distinct: Query[R]

  def union(q: Query[R]): Query[R] = error("not implemented")

  def minus(q: Query[R]): Query[R] = error("not implemented")

  def forUpdate: Query[R]
}
