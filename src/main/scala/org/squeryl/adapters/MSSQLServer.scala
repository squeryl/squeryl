/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.adapters

import java.sql.SQLException
import org.squeryl.internals.{StatementWriter, FieldMetaData}
import org.squeryl.dsl.ast._

class MSSQLServer extends GenericAdapter {

  override def isFullOuterJoinSupported = false

  override def intTypeDeclaration = "int"
  override def stringTypeDeclaration = "varchar"
  override def stringTypeDeclaration(length:Int) = "varchar("+length+")"
  override def booleanTypeDeclaration = "bit"
  override def doubleTypeDeclaration = "float"
  override def longTypeDeclaration = "bigint"
  override def bigDecimalTypeDeclaration = "decimal"
  override def bigDecimalTypeDeclaration(precision:Int, scale:Int) = "numeric(" + precision + "," + scale + ")"
  override def binaryTypeDeclaration = "varbinary(8000)"


  override def dateTypeDeclaration = "date"
  override def floatTypeDeclaration = "real"
  override def timestampTypeDeclaration = "datetime"

  override def supportsUnionQueryOptions = false

  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean): String = {

    var res = "  " + quoteIdentifier(fmd.columnName) + " " + databaseTypeFor(fmd)
    if(!fmd.isOption)
      res += " not null"

    if(isPrimaryKey)
      res += " primary key"

    if(supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      res += " IDENTITY(1,1)"

    res
  }

  override def isTableDoesNotExistException(e: SQLException): Boolean =
    e.getErrorCode == 3701

  override def writeEndOfQueryHint(isForUpdate: () => Boolean, sw: StatementWriter) = {}

  override def writeEndOfFromHint(qen: QueryExpressionElements, sw: StatementWriter) =
    if(qen.isForUpdate) {
      sw.write("with(updlock, rowlock)")
      sw.pushPendingNextLine
    }
  
  override def writeConcatFunctionCall(fn: FunctionNode, sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " + ", false)

  override def writeConcatOperator(left: ExpressionNode, right: ExpressionNode, sw: StatementWriter) = {
    val binaryOpNode = new BinaryOperatorNode(left, right, "+")
    binaryOpNode.doWrite(sw)
  }

  override def writeRegexExpression(left: ExpressionNode, pattern: String, sw: StatementWriter) = {
    // If you are keen enough you can implement a UDF and subclass this method to call out to it.
    // http://msdn.microsoft.com/en-us/magazine/cc163473.aspx
    throw new UnsupportedOperationException("MSSQL does not yet support a regex function")
  }

  override def writeQuery(qen: QueryExpressionElements, sw: StatementWriter): Unit =
    if(qen.page.isEmpty)
      super.writeQuery(qen, sw)
    else {
      val page = qen.page.get
      val beginOffset = page._1
      val pageSize = page._2

      sw.writeIndented {
        super.writeQuery(qen, sw, inverseOrderBy = false, Some(" TOP " + (beginOffset + pageSize) + " "))
      }
    }
  
  override def writePaginatedQueryDeclaration(page: () => Option[(Int, Int)], sw: StatementWriter): Unit = {}

  override def quoteIdentifier(s: String): String = "[" + s + "]"
}
