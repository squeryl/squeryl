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
 ******************************************************************************/
package org.squeryl.adapters

import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.{Session, Table}
import org.squeryl.dsl.ast._

class OracleAdapter extends DatabaseAdapter {

  override def intTypeDeclaration = "number"
  override def stringTypeDeclaration = "varchar2(255)"
  override def stringTypeDeclaration(length:Int) = "varchar2("+length+")"
  override def booleanTypeDeclaration = "number(1)"
  override def doubleTypeDeclaration = "double precision"
  override def longTypeDeclaration = "number"

  override def supportsAutoIncrementInColumnDeclaration: Boolean = false

  override def postCreateTable(s: Session, t: Table[_]) = {

    val sw = new StatementWriter(false, this)
    sw.write("create sequence ", sequenceName(t), " start with 1 increment by 1 nomaxvalue")
    val st = s.connection.createStatement
    st.execute(sw.statement)
  }

  override def postDropTable(t: Table[_]) = {
    val sw = new StatementWriter(false, this)
    sw.write("drop sequence ", sequenceName(t))
    val st = Session.currentSession.connection.createStatement
    st.execute(sw.statement)
  }

  def sequenceName(t: Table[_]) = "s_" + t.name
  
  override def writeInsert[T](o: T, t: Table[T], sw: StatementWriter):Unit = {

    val o_ = o.asInstanceOf[AnyRef]

    val autoIncPK = t.posoMetaData.fieldsMetaData.find(fmd => fmd.isAutoIncremented)

    if(autoIncPK == None) {
      super.writeInsert(o, t, sw)
      return
    }

    val f = t.posoMetaData.fieldsMetaData.filter(fmd => fmd != autoIncPK.get)

    val colNames = List(autoIncPK.get) ::: f.toList
    val colVals = List(sequenceName(t) + ".nextval") ::: f.map(fmd => writeValue(o_, fmd, sw)).toList

    sw.write("insert into ");
    sw.write(t.name);
    sw.write(" (");
    sw.write(colNames.map(fmd => fmd.columnName).mkString(", "));
    sw.write(") values ");
    sw.write(colVals.mkString("(",",",")"));
  }

  override def writeConcatFunctionCall(fn: FunctionNode[_], sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " || ", false)

  override def writeOuterJoin(oje: OuterJoinExpression, sw: StatementWriter) = {
    sw.write(oje.leftRightOrFull)
    sw.write(" outer join ")
    oje.queryableExpressionNode.write(sw)
    sw.write(" ")
    sw.write(oje.queryableExpressionNode.alias)
    sw.write(" on ")
    oje.matchExpression.write(sw)
  }

  override def writePaginatedQueryDeclaration(qen: QueryExpressionElements, sw: StatementWriter) = {} 

  override def writeQuery(qen: QueryExpressionElements, sw: StatementWriter) =
    if(qen.page == None)
      super.writeQuery(qen, sw)
    else {        
      sw.write("select sq____1.* from (")
      sw.nextLine
      sw.writeIndented {
        sw.write("select sq____0.*, rownum as rn____")
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
}
