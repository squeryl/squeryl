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

import org.squeryl.dsl.CompositeKey
import org.squeryl.dsl.ast.{ExpressionNode, QueryExpressionElements}
import org.squeryl._
import org.squeryl.internals._

import scala.collection.mutable.ArrayBuffer

class SQLiteAdapter extends DatabaseAdapter {

  override def uuidTypeDeclaration = "uuid"
  override def isFullOuterJoinSupported = false

  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    var res = "  " + fmd.columnName + " " + databaseTypeFor(fmd)

    for(d <- fmd.defaultValue) {
      val v = convertToJdbcValue(d.value.asInstanceOf[AnyRef])
      if(v.isInstanceOf[String])
        res += " default '" + v + "'"
      else
        res += " default " + v 
    }
    
    if(!fmd.isOption)
      res += " not null"

    if(isPrimaryKey)
      res += " primary key"

    if(supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      res += " autoincrement"

    res
  }

  override def writeCreateTable[T](t: Table[T], sw: StatementWriter, schema: Schema): Unit = {
    sw.write("create table ")
    sw.write(quoteName(t.prefixedName))
    sw.write(" (\n")
    sw.writeIndented {
      sw.writeLinesWithSeparator(
        t.posoMetaData.fieldsMetaData.map(
          fmd => writeColumnDeclaration(fmd, fmd.declaredAsPrimaryKeyInSchema, schema)
        ),
        ","
      )
    }
    val compositePrimaryKeys = _allCompositePrimaryKeys(t)
    if (compositePrimaryKeys.nonEmpty) {
      sw.write(", PRIMARY KEY (")
      sw.write(compositePrimaryKeys map (_.columnName) mkString ", ")
      sw.write(")")
    }

    sw.write(")")
  }

  private def _allCompositePrimaryKeys[T](t: Table[T]): Seq[FieldMetaData] = {
    (t.ked map { ked =>
      Utils.mapSampleObject(
        t.asInstanceOf[Table[AnyRef]],
        (z: AnyRef) => {
          val id = ked.asInstanceOf[KeyedEntityDef[AnyRef, AnyRef]].getId(z)
          id match {
            case key: CompositeKey => key._fields
            case _ => Seq.empty[FieldMetaData]
          }

        }
      )
    }) getOrElse Seq.empty[FieldMetaData]
  }


  override def intTypeDeclaration: String = "INTEGER"

  override def longTypeDeclaration = "INTEGER"

  override def supportsForeignKeyConstraints: Boolean = false

  override def writeCompositePrimaryKeyConstraint(t: Table[_], cols: Iterable[FieldMetaData]): String =
    s"SELECT * FROM sqlite_master WHERE 1 = 2"

  override def writeDropTable(tableName: String): String = s"DROP TABLE IF EXISTS $tableName"

  override def isTableDoesNotExistException(e: SQLException): Boolean =
    e.getErrorCode == 42102

  override def supportsCommonTableExpressions = false

  override def writeEndOfQueryHint(isForUpdate: () => Boolean, qen: QueryExpressionElements, sw: StatementWriter) =
    if(isForUpdate()) {
      sw.pushPendingNextLine
    }

  override def writeRegexExpression(left: ExpressionNode, pattern: String, sw: StatementWriter) = {
    sw.write("(")
    left.write(sw)
    sw.write(" LIKE ?)")
    sw.addParam(ConstantStatementParam(InternalFieldMapper.stringTEF.createConstant(pattern)))
  }
}
