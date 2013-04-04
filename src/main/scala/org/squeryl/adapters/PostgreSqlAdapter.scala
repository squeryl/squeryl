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

import org.squeryl.dsl.ast.FunctionNode
import java.sql.{ResultSet, SQLException}
import java.util.UUID
import org.squeryl.internals.{StatementWriter, DatabaseAdapter, FieldMetaData}
import org.squeryl.{Session, Table}

class PostgreSqlAdapter extends DatabaseAdapter {

  /**
   * NB: You can override `usePostgresSequenceNamingScheme` to return true in a
   * child class to change the sequence naming behavior to align with the
   * default postgresql scheme.
   */
  def usePostgresSequenceNamingScheme: Boolean = false

  override def intTypeDeclaration = "integer"
  override def stringTypeDeclaration = "varchar"
  override def stringTypeDeclaration(length:Int) = "varchar("+length+")"
  override def booleanTypeDeclaration = "boolean"
  override def doubleTypeDeclaration = "double precision"
  override def longTypeDeclaration = "bigint"
  override def bigDecimalTypeDeclaration = "numeric"
  override def bigDecimalTypeDeclaration(precision:Int, scale:Int) = "numeric(" + precision + "," + scale + ")"
  override def binaryTypeDeclaration = "bytea"
  override def uuidTypeDeclaration = "uuid"
    
    
  override def jdbcIntArrayCreationType = "int4"
  override def jdbcLongArrayCreationType = "int8"
  override def jdbcDoubleArrayCreationType = "float8"
  override def jdbcStringArrayCreationType = "varchar"
    
  override def foreignKeyConstraintName(foreignKeyTable: Table[_], idWithinSchema: Int) =
    foreignKeyTable.name + "FK" + idWithinSchema

  override def postCreateTable(t: Table[_], printSinkWhenWriteOnlyMode: Option[String => Unit]) = {

    val autoIncrementedFields = t.posoMetaData.fieldsMetaData.filter(_.isAutoIncremented)

    for(fmd <-autoIncrementedFields) {
      val sw = new StatementWriter(false, this)
      sw.write("create sequence ", quoteName(fmd.sequenceName))

      if(printSinkWhenWriteOnlyMode == None) {
        val st = Session.currentSession.connection.createStatement
        st.execute(sw.statement)
      }
      else
        printSinkWhenWriteOnlyMode.get.apply(sw.statement + ";")
    }
  }                                               

  def sequenceName(t: Table[_]) =
    if (usePostgresSequenceNamingScheme) {
      // This is compatible with the default postgresql sequence naming scheme.
      val autoIncPK = t.posoMetaData.fieldsMetaData.find(fmd => fmd.isAutoIncremented)
      t.name + "_" + autoIncPK.get.nameOfProperty + "_seq"
    } else {
      // Use the legacy Squeryl sequence naming scheme.
      t.prefixedPrefixedName("seq_")
    }

  override def createSequenceName(fmd: FieldMetaData) =
    if (usePostgresSequenceNamingScheme) {
      // This is compatible with the default postgresql sequence naming scheme.
      fmd.parentMetaData.viewOrTable.name + "_" + fmd.columnName + "_seq"
    } else {
      // Use the legacy Squeryl sequence naming scheme.
      super.createSequenceName(fmd)
    }

  override def writeConcatFunctionCall(fn: FunctionNode, sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " || ", false)
  
  override def writeInsert[T](o: T, t: Table[T], sw: StatementWriter): Unit = {

    val o_ = o.asInstanceOf[AnyRef]

    val autoIncPK = t.posoMetaData.fieldsMetaData.find(fmd => fmd.isAutoIncremented)

    if(autoIncPK == None) {
      super.writeInsert(o, t, sw)
      return
    }

    val f = getInsertableFields(t.posoMetaData.fieldsMetaData)

    val colNames = List(autoIncPK.get) ::: f.toList
    val colVals = List("nextval('" + quoteName(autoIncPK.get.sequenceName) + "')") ::: f.map(fmd => writeValue(o_, fmd, sw)).toList

    sw.write("insert into ");
    sw.write(quoteName(t.prefixedName));
    sw.write(" (");
    sw.write(colNames.map(fmd => quoteName(fmd.columnName)).mkString(", "));
    sw.write(") values ");
    sw.write(colVals.mkString("(",",",")"));
  }

  override def supportsAutoIncrementInColumnDeclaration: Boolean = false

  override def isTableDoesNotExistException(e: SQLException) =
   e.getSQLState.equals("42P01")

  override def writeCompositePrimaryKeyConstraint(t: Table[_], cols: Iterable[FieldMetaData]) =
  {
    // alter table TableName add primary key (col1, col2) ;
    val sb = new StringBuilder(256)
    sb.append("alter table ")
    sb.append(quoteName(t.prefixedName))
    sb.append(" add primary key (")
    sb.append(cols.map(_.columnName).map(quoteName(_)).mkString(","))
    sb.append(")")
    sb.toString
  }


  override def writeDropForeignKeyStatement(foreignKeyTable: Table[_], fkName: String) =
    "alter table " + quoteName(foreignKeyTable.prefixedName) + " drop constraint " + quoteName(fkName)

  override def failureOfStatementRequiresRollback = true
  
  override def postDropTable(t: Table[_]) = {
    
    val autoIncrementedFields = t.posoMetaData.fieldsMetaData.filter(_.isAutoIncremented)

    for(fmd <-autoIncrementedFields) {
      execFailSafeExecute("drop sequence " + quoteName(fmd.sequenceName), e=>e.getSQLState.equals("42P01"))
    }
  }

  override def quoteIdentifier(s: String) = List("\"", s.replace("\"", "\"\""), "\"").mkString

  override def convertFromUuidForJdbc(u: UUID): AnyRef = u
  override def convertToUuidForJdbc(rs: ResultSet, i: Int): UUID = rs.getObject(i).asInstanceOf[UUID]
}
