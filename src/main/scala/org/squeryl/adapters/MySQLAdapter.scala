package org.squeryl.adapters

import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.dsl.ast.QueryExpressionElements

class MySQLAdapter extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def floatTypeDeclaration = "float"

  //override def nvlToken = "ifnull"
}
