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
import org.squeryl.internals.{StatementWriter, FieldMetaData, DatabaseAdapter}
import org.squeryl.dsl.ast._
import org.squeryl.{Schema}

class MSSQLServer extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def intTypeDeclaration = "int"
  override def stringTypeDeclaration = "varchar(255)"
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
  
  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    var res = "  " + fmd.columnName + " " + databaseTypeFor(fmd)
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

  override def writeEndOfQueryHint(qen: QueryExpressionElements, sw: StatementWriter) = {}

  override def writeEndOfFromHint(qen: QueryExpressionElements, sw: StatementWriter) =
    if(qen.isForUpdate) {
      sw.write("with(updlock, rowlock)")
      sw.pushPendingNextLine
    }
  
  override def writeConcatFunctionCall(fn: FunctionNode[_], sw: StatementWriter) =
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

//SELECT TOP <pageSize> CustomerID,CompanyName,ContactName,ContactTitle
//  FROM
//  (SELECT TOP <currentPageNumber * pageSize>
//          CustomerID,CompanyName,ContactName,ContactTitle
//   FROM
//     Customers AS T1 ORDER BY ContactName DESC)
//  AS T2 ORDER BY ContactName ASC

//  override def writeQuery(qen: QueryExpressionElements, sw: StatementWriter) =
//    if(qen.page == None)
//      super.writeQuery(qen, sw)
//    else {
//      val page = qen.page.get
//      val beginOffset = page._1
//      val pageSize = page._2
//      //val endOffset = pageSize + beginOffset
//      val sl = qen.selectList.filter(e => ! e.inhibited)
//      val ob =
//        if(! qen.orderByClause.isEmpty && qen.parent == None )
//          qen.orderByClause.filter(e => ! e.inhibited)
//        else
//          Nil
//
//      val obInverse = ob.map(_.asInstanceOf[OrderByExpression].inverse)
//
//      sw.write("select * from (")
//      sw.nextLine
//      sw.write("select TOP " + pageSize + " * ")
//      sw.nextLine
//      sw.write("from (")
//      sw.nextLine
//      sw.writeIndented {
//        super.writeQuery(qen, sw, false, Some(" TOP " + (beginOffset + pageSize) + " "))
//      }
//      sw.write(") _____z1_____")
//      if(ob != Nil) {
//        sw.nextLine
//        sw.write(" order by ")
//        sw.write(ob.map(_.asInstanceOf[OrderByExpression].inverse).map(_.writeToString.replace('.','_')).mkString(","))
//      }
//      sw.write(") _____z2_____")
//      if(ob != Nil) {
//        sw.nextLine
//        sw.write(" order by ")
//        sw.write(ob.map(_.writeToString.replace('.','_')).mkString(","))
//      }
//
//      println(sw.statement)
//    }

  override def writeQuery(qen: QueryExpressionElements, sw: StatementWriter) =
    if(qen.page == None)
      super.writeQuery(qen, sw)
    else {
      val page = qen.page.get
      val beginOffset = page._1
      val pageSize = page._2

      sw.write("With ___z____ as (")
      sw.writeIndented {
        super.writeQuery(qen, sw, false, Some(" TOP " + (beginOffset + pageSize) + " "))
      }
      sw.write(")")
    }
  
  private def _stripPrefix(selectE: String):String = {
    val i = selectE.lastIndexOf(" as ")
    selectE.substring(i + 4, selectE.length)
  }
  
  override def writePaginatedQueryDeclaration(qen: QueryExpressionElements, sw: StatementWriter):Unit = {}
}
