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

import org.squeryl.{ReferentialAction, Table}
import java.sql.SQLException
import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.dsl.ast.{BinaryOperatorNode, ExpressionNode}

class MySQLAdapter extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def floatTypeDeclaration = "float"

  override def binaryTypeDeclaration = "blob"

  override def timestampTypeDeclaration = "datetime"
  
  override def writeForeignKeyDeclaration(
    foreignKeyTable: Table[_], foreignKeyColumnName: String,
    primaryKeyTable: Table[_], primaryKeyColumnName: String,
    referentialAction1: Option[ReferentialAction],
    referentialAction2: Option[ReferentialAction],
    fkId: Int) = {

    val sb = new StringBuilder(256)

    sb.append("alter table ")
    sb.append(foreignKeyTable.prefixedName)
    sb.append(" add constraint ")
    sb.append(foreignKeyConstraintName(foreignKeyTable, fkId))
    sb.append(" foreign key (")
    sb.append(foreignKeyColumnName)
    sb.append(") references ")
    sb.append(primaryKeyTable.prefixedName)
    sb.append("(")
    sb.append(primaryKeyColumnName)
    sb.append(")")
    
    val f =  (ra:ReferentialAction) => {
      sb.append(" on ")
      sb.append(ra.event)
      sb.append(" ")
      sb.append(ra.action)
    }

    referentialAction1.foreach(f)
    referentialAction2.foreach(f)

    sb.toString
  }

  override def writeDropForeignKeyStatement(foreignKeyTable: Table[_], fkName: String) =
    "alter table " + foreignKeyTable.prefixedName + " drop foreign key " + fkName

  override def isTableDoesNotExistException(e: SQLException) =
    e.getErrorCode == 1051 

  /**
   *
   * Foreign key constraints are not supported,
   *
   *  MySQL has some pre requisites for creating a foreign key constraint
   *  one of which is :
   *
   *  -> The foreign key can be self referential (referring to the same table). When you add a foreign key constraint to a table using ALTER TABLE, remember to create the required indexes first.
   *
   *  http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html
   *
   *  Apparently there are other pre requisites, because creating foreign key constraints still gives :
   *
   * 		Time	Action	Response	Duration / Fetch
   *  0	1	18:26:25	alter table CourseSubscription add constraint CourseSubscriptionFK3
   *  foreign key (courseId) references Course(id)	Error Code: 1005
   *  Can't create table 'test.#sql-57c_42' (errno: 150)
   *
   * 
   *  http://bytes.com/topic/mysql/answers/865699-cant-create-table-errno-150-foreign-key-constraints
   *
   * 
   */

  override def supportsForeignKeyConstraints = false

  override def writeRegexExpression(left: ExpressionNode, pattern: String, sw: StatementWriter) = {
    sw.write("(")
    left.write(sw)
    sw.write(" regexp ?)")
    sw.addParam(pattern)
  }

  override def writeConcatOperator(left: ExpressionNode, right: ExpressionNode, sw: StatementWriter) = {
    sw.write("concat(")
    left.write(sw)
    sw.write(",")
    right.write(sw)
    sw.write(")")
  }
}
