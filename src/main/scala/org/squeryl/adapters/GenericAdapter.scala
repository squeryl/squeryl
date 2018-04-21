package org.squeryl.adapters

import java.sql.SQLException

import org.squeryl.Table
import org.squeryl.dsl.ast.QueryExpressionElements
import org.squeryl.internals.{DatabaseAdapter, StatementWriter}


abstract class GenericAdapter extends DatabaseAdapter {

  override def postCreateTable(t: Table[_], printSinkWhenWriteOnlyMode: Option[String => Unit]): Unit = {}

  override def postDropTable(t: Table[_]): Unit = {}

  override def isNotNullConstraintViolation(e: SQLException): Boolean = false

  override def writeEndOfFromHint(qen: QueryExpressionElements, sw: StatementWriter): Unit = {}

}
