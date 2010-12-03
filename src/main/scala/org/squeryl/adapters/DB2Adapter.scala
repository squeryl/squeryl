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

import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.{Session, Table}
import java.sql.SQLException
import org.squeryl.dsl.ast._

class DB2Adapter extends DatabaseAdapter {

  override def booleanTypeDeclaration = "char(1)"

  override def timestampTypeDeclaration = "timestamp"

  // This defaults to 1M in size.
  override def binaryTypeDeclaration = "blob"

  override def supportsAutoIncrementInColumnDeclaration: Boolean = false

  override def postCreateTable(t: Table[_], printSinkWhenWriteOnlyMode: Option[String => Unit]) = {

    val sw = new StatementWriter(false, this)
    sw.write("create sequence ", sequenceName(t), " start with 1 increment by 1 nomaxvalue")

    if(printSinkWhenWriteOnlyMode == None) {
      val st = Session.currentSession.connection.createStatement
      st.execute(sw.statement)
    }
    else
      printSinkWhenWriteOnlyMode.get.apply(sw.statement + ";")
  }

  override def postDropTable(t: Table[_]) =
    execFailSafeExecute("drop sequence " + sequenceName(t), e => e.getErrorCode == -204)

  def sequenceName(t: Table[_]) =
    t.prefixedPrefixedName("s_")

  override def writeInsert[T](o: T, t: Table[T], sw: StatementWriter): Unit = {

    val o_ = o.asInstanceOf[AnyRef]

    val autoIncPK = t.posoMetaData.fieldsMetaData.find(fmd => fmd.isAutoIncremented)

    if (autoIncPK == None) {
      super.writeInsert(o, t, sw)
      return
    }

    val f = t.posoMetaData.fieldsMetaData.filter(fmd => fmd != autoIncPK.get)

    val colNames = List(autoIncPK.get) ::: f.toList
    val colVals = List("next value for " + sequenceName(t)) ::: f.map(fmd => writeValue(o_, fmd, sw)).toList

    sw.write("insert into ");
    sw.write(t.prefixedName);
    sw.write(" (");
    sw.write(colNames.map(fmd => fmd.columnName).mkString(", "));
    sw.write(") values ");
    sw.write(colVals.mkString("(", ",", ")"));
  }

  override def writeConcatFunctionCall(fn: FunctionNode[_], sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " || ", false)

  override def isTableDoesNotExistException(e: SQLException) = {
    e.getErrorCode == -204
  }

  override def writePaginatedQueryDeclaration(qen: QueryExpressionElements, sw: StatementWriter) = {}

  override def writeQuery(qen: QueryExpressionElements, sw: StatementWriter) =
    if (qen.page == None)
      super.writeQuery(qen, sw)
    else {
      sw.write("select sq____1.* from (")
      sw.nextLine
      sw.writeIndented {
        sw.write("select sq____0.*, row_number() over() as rn____")
        sw.nextLine
        sw.write("from")
        sw.nextLine
        sw.writeIndented {
          sw.write("(")
          super.writeQuery(qen, sw)
          sw.write(") sq____0")
        }
      }
      sw.nextLine
      sw.write(") sq____1")
      sw.nextLine
      sw.write("where")
      sw.nextLine
      sw.writeIndented {
        sw.write("rn____ between ")
        val page = qen.page.get
        val beginOffset = page._1 + 1
        val endOffset = page._2 + beginOffset - 1
        sw.write(beginOffset.toString)
        sw.write(" and ")
        sw.write(endOffset.toString)
      }
    }

  override def writeConcatOperator(left: ExpressionNode, right: ExpressionNode, sw: StatementWriter) = {
    sw.write("(")
    _writeConcatOperand(left, sw)
    sw.write(" ")
    sw.write("||")
    sw.write(" ")
    _writeConcatOperand(right, sw)
    sw.write(")")
  }

  private def _writeConcatOperand(e: ExpressionNode, sw: StatementWriter) = {
    if (e.isInstanceOf[ConstantExpressionNode[_]]) {
      val c = e.asInstanceOf[ConstantExpressionNode[Any]]
      sw.write("cast(")
      e.write(sw)
      sw.write(" as varchar(")
      sw.write(c.value.toString.length.toString)
      sw.write("))")
    }
    else
      e.write(sw)
  }

  override def writeRegexExpression(left: ExpressionNode, pattern: String, sw: StatementWriter) = {
    // If you are keen enough you can implement a UDF and subclass this method to call out to it.
    // See http://www.ibm.com/developerworks/data/library/techarticle/0301stolze/0301stolze.html for how.
    throw new UnsupportedOperationException("DB2 does not support a regex scalar function")
  }

}