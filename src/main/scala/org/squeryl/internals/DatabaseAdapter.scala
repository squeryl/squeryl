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
package org.squeryl.internals

import org.squeryl.dsl.ast._
import org.squeryl._
import dsl.CompositeKey
import org.squeryl.{Schema, Session, Table}
import java.sql._
import java.util.UUID

trait DatabaseAdapter {

  class Zip[T](val element: T, val isLast: Boolean, val isFirst: Boolean)

  class ZipIterable[T](iterable: Iterable[T]) {
    val count = iterable.size
    def foreach[U](f: Zip[T] => U): Unit = {
      var c = 1
      for (i <- iterable) {
        f(new Zip(i, c == count, c == 1))
        c += 1
      }
    }

    def zipi = this
  }

  implicit def zipIterable[T](i: Iterable[T]): ZipIterable[T] = new ZipIterable(i)

  def writeQuery(qen: QueryExpressionElements, sw: StatementWriter): Unit =
    writeQuery(qen, sw, false, None)

  /**
   * Should we verify that when we delete by primary key the JDBC driver reports
   * that no more than one row was affected?  MonetDB in particular seems to
   * act badly here
   * @return true if we should throw an exception if the driver reports more than 1 affected row
   */
  def verifyDeleteByPK: Boolean = true

  protected[squeryl] def writeCteReference(sw: StatementWriter, q: QueryExpressionElements): Unit = {
    sw.write(quoteName("cte_" + q.alias))
  }

  protected def writeQuery(
    qen: QueryExpressionElements,
    sw: StatementWriter,
    inverseOrderBy: Boolean,
    topHint: Option[String]
  ): Unit = {

    if (supportsCommonTableExpressions && qen.commonTableExpressions.nonEmpty) {
      sw.write("With")
      for (z <- qen.commonTableExpressions.zipi) {
        sw.write(" ")
        writeCteReference(sw, z.element)
        sw.write(" As ")
        sw.write("(")
        writeQuery(z.element, sw)
        sw.write(")")
        if (!z.isLast) {
          sw.write(",")
        }
        sw.nextLine
      }
    }

    sw.write("Select")

    topHint.foreach(" " + sw.write(_) + " ")

    if (qen.selectDistinct)
      sw.write(" distinct")

    sw.nextLine
    sw.writeIndented {
      sw.writeNodesWithSeparator(qen.selectList.filter(e => !e.inhibited), ",", true)
    }
    sw.nextLine
    sw.write("From")
    sw.nextLine

    if (!qen.isJoinForm) {
      sw.writeIndented {
        for (z <- qen.tableExpressions.zipi) {
          z.element.write(sw)
          sw.write(" ")
          sw.write(sw.quoteName(z.element.alias))
          if (!z.isLast) {
            sw.write(",")
            sw.nextLine
          }
        }
        sw.pushPendingNextLine
      }
    } else {
      val singleNonJoinTableExpression = qen.tableExpressions.filter(!_.isMemberOfJoinList)
      assert(
        singleNonJoinTableExpression.size == 1,
        "join query must have exactly one FROM argument, got : " + qen.tableExpressions
      )
      val firstJoinExpr = singleNonJoinTableExpression.head
      val restOfJoinExpr = qen.tableExpressions.filter(_.isMemberOfJoinList)
      firstJoinExpr.write(sw)
      sw.write(" ")
      sw.write(sw.quoteName(firstJoinExpr.alias))
      sw.nextLine

      for (z <- restOfJoinExpr.zipi) {
        writeJoin(z.element, sw)
        if (z.isLast)
          sw.unindent
        sw.pushPendingNextLine
      }
    }

    writeEndOfFromHint(qen, sw)

    if (qen.hasUnInhibitedWhereClause) {
      sw.write("Where")
      sw.nextLine
      sw.writeIndented {
        qen.whereClause.get.write(sw)
      }
      sw.pushPendingNextLine
    }

    if (qen.groupByClause.exists(e => !e.inhibited)) {
      sw.write("Group By")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.groupByClause.filter(e => !e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }

    if (qen.havingClause.exists(e => !e.inhibited)) {
      sw.write("Having")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.havingClause.filter(e => !e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }

    if (qen.orderByClause.exists(e => !e.inhibited)) {
      sw.write("Order By")
      sw.nextLine
      val ob0 = qen.orderByClause.filter(e => !e.inhibited)
      val ob = if (inverseOrderBy) ob0.map(_.asInstanceOf[OrderByExpression].inverse) else ob0
      sw.writeIndented {
        sw.writeNodesWithSeparator(ob, ",", true)
      }
      sw.pushPendingNextLine
    }

    writePaginatedQueryDeclaration(() => qen.page, qen, sw)

    writeEndOfQueryHint(() => qen.isForUpdate, qen, sw)
  }

  def writeUnionQueryOptions(qen: QueryExpressionElements, sw: StatementWriter): Unit = {
    if (!supportsUnionQueryOptions)
      Utils.throwError("Database adapter does not support query options on a union query")

    writeEndOfQueryHint(() => qen.unionIsForUpdate, qen, sw)
    writePaginatedQueryDeclaration(() => qen.unionPage, qen, sw)
  }

  def writeEndOfQueryHint(isForUpdate: () => Boolean, qen: QueryExpressionElements, sw: StatementWriter) =
    if (isForUpdate()) {
      sw.write("for update")
      sw.pushPendingNextLine
    }

  def writeEndOfFromHint(qen: QueryExpressionElements, sw: StatementWriter) = {}

  def writePaginatedQueryDeclaration(
    page: () => Option[(Int, Int)],
    qen: QueryExpressionElements,
    sw: StatementWriter
  ): Unit =
    page().foreach(p => {
      sw.write("limit ")
      sw.write(p._2.toString)
      sw.write(" offset ")
      sw.write(p._1.toString)
      sw.pushPendingNextLine
    })

  def writeJoin(queryableExpressionNode: QueryableExpressionNode, sw: StatementWriter) = {
    sw.write(queryableExpressionNode.joinKind.get._1)
    sw.write(" ")
    sw.write(queryableExpressionNode.joinKind.get._2)
    sw.write(" join ")
    queryableExpressionNode.write(sw)
    sw.write(" as ")
    sw.write(sw.quoteName(queryableExpressionNode.alias))
    sw.write(" on ")
    queryableExpressionNode.joinExpression.get.write(sw)
  }

  def intTypeDeclaration = "int"
  def stringTypeDeclaration = "varchar"
  def stringTypeDeclaration(length: Int) = "varchar(" + length + ")"
  def booleanTypeDeclaration = "boolean"
  def doubleTypeDeclaration = "double"
  def dateTypeDeclaration = "date"
  def longTypeDeclaration = "bigint"
  def floatTypeDeclaration = "real"
  def bigDecimalTypeDeclaration = "decimal"
  def bigDecimalTypeDeclaration(precision: Int, scale: Int) = "decimal(" + precision + "," + scale + ")"
  def timestampTypeDeclaration = "timestamp"
  def binaryTypeDeclaration = "binary"
  def uuidTypeDeclaration = "char(36)"
  def intArrayTypeDeclaration = intTypeDeclaration + "[]"
  def longArrayTypeDeclaration = longTypeDeclaration + "[]"
  def doubleArrayTypeDeclaration = doubleTypeDeclaration + "[]"

  def stringArrayTypeDeclaration = stringTypeDeclaration + "[]"
  def jdbcIntArrayCreationType = intTypeDeclaration
  def jdbcLongArrayCreationType = longTypeDeclaration
  def jdbcDoubleArrayCreationType = doubleTypeDeclaration
  def jdbcStringArrayCreationType = stringTypeDeclaration

  final def arrayCreationType(ptype: Class[_]): String = {
    val rv = ptype.getName() match {
      case "java.lang.Integer" => jdbcIntArrayCreationType
      case "java.lang.Double" => jdbcDoubleArrayCreationType
      case "java.lang.Long" => jdbcLongArrayCreationType
      case "java.lang.String" => jdbcStringArrayCreationType
      case _ => ""
    }
    rv
  }

  /*
  private val _declarationHandler = new FieldTypeHandler[String] {

    def handleIntType = intTypeDeclaration
    def handleStringType  = stringTypeDeclaration
    def handleBooleanType = booleanTypeDeclaration
    def handleDoubleType = doubleTypeDeclaration
    def handleDateType = dateTypeDeclaration
    def handleLongType = longTypeDeclaration
    def handleFloatType = floatTypeDeclaration
    def handleBigDecimalType = bigDecimalTypeDeclaration
    def handleTimestampType = timestampTypeDeclaration
    def handleBinaryType = binaryTypeDeclaration
    def handleUuidType = uuidTypeDeclaration
    def handleEnumerationValueType = intTypeDeclaration
    def handleUnknownType(c: Class[_]) =
      org.squeryl.internals.Utils.throwError("don't know how to map field type " + c.getName)
  }
   */
  def databaseTypeFor(fmd: FieldMetaData): String =
    fmd.explicitDbTypeDeclaration.getOrElse(
      fmd.schema.columnTypeFor(fmd, fmd.parentMetaData.viewOrTable.asInstanceOf[Table[_]]).getOrElse {
        val nativeJdbcType = fmd.nativeJdbcType

        if (classOf[String].isAssignableFrom(nativeJdbcType))
          stringTypeDeclaration(fmd.length)
        else if (classOf[BigDecimal].isAssignableFrom(nativeJdbcType))
          bigDecimalTypeDeclaration(fmd.length, fmd.scale)
        else
          databaseTypeFor(fmd.schema.fieldMapper, nativeJdbcType)
      }
    )

  def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    val dbTypeDeclaration = databaseTypeFor(fmd)

    val sb = new java.lang.StringBuilder(128)

    sb.append("  ")
    sb.append(quoteName(fmd.columnName))
    sb.append(" ")
    sb.append(dbTypeDeclaration)

    for (d <- fmd.defaultValue) {
      sb.append(" default ")

      val v = convertToJdbcValue(d.value.asInstanceOf[AnyRef])
      if (v.isInstanceOf[String])
        sb.append("'" + v + "'")
      else
        sb.append(v)
    }

    if (isPrimaryKey)
      sb.append(" primary key")

    if (!fmd.isOption)
      sb.append(" not null")

    if (supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      sb.append(" auto_increment")

    sb.toString
  }

  def supportsAutoIncrementInColumnDeclaration: Boolean = true

  def supportsUnionQueryOptions = true

  def supportsCommonTableExpressions = true

  def writeCreateTable[T](t: Table[T], sw: StatementWriter, schema: Schema) = {

    sw.write("create table ")
    sw.write(quoteName(t.prefixedName))
    sw.write(" (\n");
    sw.writeIndented {
      sw.writeLinesWithSeparator(
        t.posoMetaData.fieldsMetaData.map(fmd => writeColumnDeclaration(fmd, fmd.declaredAsPrimaryKeyInSchema, schema)),
        ","
      )
    }
    sw.write(")")
  }

  def fillParamsInto(params: Iterable[StatementParam], s: PreparedStatement): Unit = {
    var i = 1;
    for (p <- params) {
      setParamInto(s, p, i)
      i += 1
    }
  }

  def setParamInto(s: PreparedStatement, p: StatementParam, i: Int) =
    p match {
      case ConstantStatementParam(constantTypedExpression) =>
        s.setObject(i, convertToJdbcValue(constantTypedExpression.nativeJdbcValue))
      case FieldStatementParam(o, fieldMetaData) =>
        s.setObject(i, convertToJdbcValue(fieldMetaData.getNativeJdbcValue(o)))
      case ConstantExpressionNodeListParam(v, constantExpressionNodeList) => s.setObject(i, convertToJdbcValue(v))
    }

  private def _exec[A](
    s: AbstractSession,
    sw: StatementWriter,
    block: Iterable[StatementParam] => A,
    args: Iterable[StatementParam]
  ): A =
    try {
      if (s.isLoggingEnabled)
        s.log(sw.toString)
      block(args)
    } catch {
      case e: SQLException =>
        throw SquerylSQLException(
          "Exception while executing statement : " + e.getMessage +
            "\nerrorCode: " +
            e.getErrorCode + ", sqlState: " + e.getSQLState + "\n" +
            sw.statement + "\njdbcParams:" +
            args.mkString("[", ",", "]"),
          e
        )
    }

  def failureOfStatementRequiresRollback = false

  /**
   * Some methods like 'dropTable' issue their statement, and will silence the exception.
   * For example dropTable will silence when isTableDoesNotExistException(theExceptionThrown).
   * It must be used carefully, and an exception should not be silenced unless identified.
   */
  protected def execFailSafeExecute(sw: StatementWriter, silenceException: SQLException => Boolean): Unit = {
    val s = Session.currentSession
    val c = s.connection
    val stat = createStatement(c)
    val sp =
      if (failureOfStatementRequiresRollback) Some(c.setSavepoint)
      else None

    try {
      if (s.isLoggingEnabled)
        s.log(sw.toString)
      stat.execute(sw.statement)
    } catch {
      case e: SQLException =>
        if (silenceException(e))
          sp.foreach(c.rollback(_))
        else
          throw SquerylSQLException(
            "Exception while executing statement,\n" +
              "SQLState:" + e.getSQLState + ", ErrorCode:" + e.getErrorCode + "\n :" +
              sw.statement,
            e
          )
    } finally {
      sp.foreach(c.releaseSavepoint(_))
      Utils.close(stat)
    }
  }

  implicit def string2StatementWriter(s: String): StatementWriter = {
    val sw = new StatementWriter(this)
    sw.write(s)
    sw
  }

  protected def exec[A](s: AbstractSession, sw: StatementWriter)(block: Iterable[StatementParam] => A): A = {
    _exec[A](s, sw, block, sw.params)
  }

  protected def prepareStatement(conn: Connection, statement: String): PreparedStatement =
    conn.prepareStatement(statement)

  protected def createStatement(conn: Connection): Statement =
    conn.createStatement()

  def executeQuery(s: AbstractSession, sw: StatementWriter) = exec(s, sw) { params =>
    val st = prepareStatement(s.connection, sw.statement)
    fillParamsInto(params, st)
    (st.executeQuery, st)
  }

  def executeUpdate(s: AbstractSession, sw: StatementWriter): (Int, PreparedStatement) = exec(s, sw) { params =>
    val st = prepareStatement(s.connection, sw.statement)
    fillParamsInto(params, st)
    (st.executeUpdate, st)
  }

  def executeUpdateAndCloseStatement(s: AbstractSession, sw: StatementWriter): Int = exec(s, sw) { params =>
    val st = prepareStatement(s.connection, sw.statement)
    fillParamsInto(params, st)
    try {
      st.executeUpdate
    } finally {
      st.close
    }
  }

  def executeUpdateForInsert(s: AbstractSession, sw: StatementWriter, ps: PreparedStatement) = exec(s, sw) { params =>
    fillParamsInto(params, ps)
    ps.executeUpdate
  }

  protected def getInsertableFields(fmd: Iterable[FieldMetaData]) =
    fmd.filter(fmd => !fmd.isAutoIncremented && fmd.isInsertable)

  def writeInsert[T](o: T, t: Table[T], sw: StatementWriter): Unit = {

    val o_ = o.asInstanceOf[AnyRef]
    val f = getInsertableFields(t.posoMetaData.fieldsMetaData)

    sw.write("insert into ");
    sw.write(quoteName(t.prefixedName));
    sw.write(" (");
    sw.write(f.map(fmd => quoteName(fmd.columnName)).mkString(", "));
    sw.write(") values ");
    sw.write(f.map(fmd => writeValue(o_, fmd, sw)).mkString("(", ",", ")"));
  }

  /**
   * Converts field instances so they can be fed, and understood by JDBC
   * will not do conversion from None/Some, so @arg r should be a java primitive type or
   * a CustomType
   */
  def convertToJdbcValue(r: AnyRef): AnyRef = {

    if (r == null)
      return r

    var v = r

    v match {
      case product: Product1[_] =>
        v = product._1.asInstanceOf[AnyRef]
      case _ =>
    }

    v match {
      case x: java.util.Date if (!v.isInstanceOf[java.sql.Date] && !v.isInstanceOf[Timestamp]) =>
        v = new java.sql.Date(x.getTime)
      case x: scala.math.BigDecimal =>
        v = x.bigDecimal
      case x: scala.Enumeration#Value =>
        v = x.id.asInstanceOf[AnyRef]
      case x: java.util.UUID =>
        v = convertFromUuidForJdbc(x)
      case _ =>
    }

    v
  }

//  see comment in def convertFromBooleanForJdbc
//    if(v.isInstanceOf[java.lang.Boolean])
//      v = convertFromBooleanForJdbc(v)

  // TODO: move to StatementWriter ?
  protected def writeValue(o: AnyRef, fmd: FieldMetaData, sw: StatementWriter): String =
    if (sw.isForDisplay) {
      val v = fmd.getNativeJdbcValue(o)
      if (v != null)
        v.toString
      else
        "null"
    } else {
      sw.addParam(FieldStatementParam(o, fmd))
      "?"
    }

//  protected def writeValue(sw: StatementWriter, v: AnyRef):String =
//    if(sw.isForDisplay) {
//      if(v != null)
//        v.toString
//      else
//        "null"
//    }
//    else {
//      sw.addParam(convertToJdbcValue(v))
//      "?"
//    }

  /**
   * When @arg printSinkWhenWriteOnlyMode is not None, the adapter will not execute any statement, but only silently give it to the String=>Unit closure
   */
  def postCreateTable(t: Table[_], printSinkWhenWriteOnlyMode: Option[String => Unit]) = {}

  def postDropTable(t: Table[_]) = {}

  def createSequenceName(fmd: FieldMetaData) =
    "s_" + fmd.parentMetaData.viewOrTable.name + "_" + fmd.columnName

  def writeConcatFunctionCall(fn: FunctionNode, sw: StatementWriter) = {
    sw.write(fn.name)
    sw.write("(")
    sw.writeNodesWithSeparator(fn.args, ",", false)
    sw.write(")")
  }

  def isFullOuterJoinSupported = true

  def writeUpdate[T](o: T, t: Table[T], sw: StatementWriter, checkOCC: Boolean) = {

    val o_ = o.asInstanceOf[AnyRef]

    sw.write("update ", quoteName(t.prefixedName), " set ")
    sw.nextLine
    sw.indent
    sw.writeLinesWithSeparator(
      t.posoMetaData.fieldsMetaData
        .filter(fmd => !fmd.isIdFieldOfKeyedEntity && fmd.isUpdatable)
        .map(fmd => {
          if (fmd.isOptimisticCounter)
            quoteName(fmd.columnName) + " = " + quoteName(fmd.columnName) + " + 1 "
          else
            quoteName(fmd.columnName) + " = " + writeValue(o_, fmd, sw)
        }),
      ","
    )
    sw.unindent
    sw.write("where")
    sw.nextLine
    sw.indent

    t.posoMetaData.primaryKey
      .getOrElse(
        throw new UnsupportedOperationException(
          "writeUpdate was called on an object that does not extend from KeyedEntity[]"
        )
      )
      .fold(
        pkMd => {
          val (op, vl) =
            if (pkMd.getNativeJdbcValue(o_) == null) (" is ", "null") else (" = ", writeValue(o_, pkMd, sw))
          sw.write(quoteName(pkMd.columnName), op, vl)
        },
        pkGetter => {
          Utils.createQuery4WhereClause(
            t,
            (t0: T) => {
              val ck = pkGetter.invoke(t0).asInstanceOf[CompositeKey]

              val fieldWhere = ck._fields map {
                case fmd if (fmd.getNativeJdbcValue(o_) == null) =>
                  quoteName(fmd.columnName) + " is null"
                case fmd =>
                  quoteName(fmd.columnName) + " = " + writeValue(o_, fmd, sw)
              }
              sw.write(fieldWhere.mkString(" and "))

              new EqualityExpression(
                InternalFieldMapper.intTEF.createConstant(1),
                InternalFieldMapper.intTEF.createConstant(1)
              )
            }
          )
        }
      )

    if (checkOCC)
      t.posoMetaData.optimisticCounter.foreach(occ => {
        sw.write(" and ")
        sw.write(quoteName(occ.columnName))
        sw.write(" = ")
        sw.write(writeValue(o_, occ, sw))
      })
  }

  def writeDelete[T](t: Table[T], whereClause: Option[ExpressionNode], sw: StatementWriter) = {

    sw.write("delete from ")
    sw.write(quoteName(t.prefixedName))
    if (whereClause.isDefined) {
      sw.nextLine
      sw.write("where")
      sw.nextLine
      sw.writeIndented {
        whereClause.get.write(sw)
      }
    }
  }

  /**
   * unused at the moment, since all jdbc drivers adhere to the standard that :
   *  1 == true, false otherwise. If a new driver would not adhere
   * to this, the call can be uncommented in method convertToJdbcValue
   */
  def convertFromBooleanForJdbc(b: Boolean): Boolean = b

  /**
   * unused for the same reason as def convertFromBooleanForJdbc (see comment)
   */
  def convertToBooleanForJdbc(rs: ResultSet, i: Int): Boolean = rs.getBoolean(i)

  def convertFromUuidForJdbc(u: UUID): AnyRef =
    u.toString

  def convertToUuidForJdbc(rs: ResultSet, i: Int): UUID =
    UUID.fromString(rs.getString(i))

  def writeUpdate(t: Table[_], us: UpdateStatement, sw: StatementWriter) = {

    val colsToUpdate = us.columns.iterator

    sw.write("update ")
    sw.write(quoteName(t.prefixedName))
    sw.write(" set")
    sw.indent
    sw.nextLine
    for (z <- us.values.zipi) {
      val col = colsToUpdate.next()
      sw.write(quoteName(col.columnName))
      sw.write(" = ")
      val v = z.element
      col.explicitDbTypeDeclaration match {
        case Some(dbType) if col.explicitDbTypeCast => {
          sw.write("cast(")
          v.write(sw)
          sw.write(s" as ${sw.quoteName(dbType)})")
        }
        case _ => {
          sw.write("(")
          v.write(sw)
          sw.write(")")
        }
      }
      if (!z.isLast) {
        sw.write(",")
        sw.nextLine
      }
    }

    if (t.posoMetaData.isOptimistic) {
      sw.write(",")
      sw.nextLine
      val occ = t.posoMetaData.optimisticCounter.get
      sw.write(quoteName(occ.columnName))
      sw.write(" = ")
      sw.write(quoteName(occ.columnName) + " + 1")
    }

    sw.unindent

    if (us.whereClause.isDefined) {
      sw.nextLine
      sw.write("Where")
      sw.nextLine
      sw.writeIndented {
        us.whereClause.get.write(sw)
      }
    }
  }

  def nvlToken = "coalesce"

  def writeNvlCall(left: ExpressionNode, right: ExpressionNode, sw: StatementWriter) = {
    sw.write(nvlToken)
    sw.write("(")
    left.write(sw)
    sw.write(",")
    right.write(sw)
    sw.write(")")
  }

  /**
   * Figures out from the SQLException (ex.: vendor specific error code) 
   * if it's cause is a NOT NULL constraint violation
   */
  def isNotNullConstraintViolation(e: SQLException): Boolean = false

  def foreignKeyConstraintName(foreignKeyTable: Table[_], idWithinSchema: Int) =
    foreignKeyTable.name + "FK" + idWithinSchema

  def viewAlias(vn: ViewExpressionNode[_]) =
    if (vn.view.prefix.isDefined)
      vn.view.prefix.get + "_" + vn.view.name + vn.uniqueId.get
    else
      vn.view.name + vn.uniqueId.get

  def writeForeignKeyDeclaration(
    foreignKeyTable: Table[_],
    foreignKeyColumnName: String,
    primaryKeyTable: Table[_],
    primaryKeyColumnName: String,
    referentialAction1: Option[ReferentialAction],
    referentialAction2: Option[ReferentialAction],
    fkId: Int
  ) = {

    val sb = new java.lang.StringBuilder(256)

    sb.append("alter table ")
    sb.append(quoteName(foreignKeyTable.prefixedName))
    sb.append(" add constraint ")
    sb.append(quoteName(foreignKeyConstraintName(foreignKeyTable, fkId)))
    sb.append(" foreign key (")
    sb.append(quoteName(foreignKeyColumnName))
    sb.append(") references ")
    sb.append(quoteName(primaryKeyTable.prefixedName))
    sb.append("(")
    sb.append(quoteName(primaryKeyColumnName))
    sb.append(")")

    val f = (ra: ReferentialAction) => {
      sb.append(" on ")
      sb.append(ra.event)
      sb.append(" ")
      sb.append(ra.action)
    }

    referentialAction1.foreach(f)
    referentialAction2.foreach(f)

    sb.toString
  }

  protected def currenSession =
    Session.currentSession

  def writeDropForeignKeyStatement(foreignKeyTable: Table[_], fkName: String) =
    "alter table " + quoteName(foreignKeyTable.prefixedName) + " drop constraint " + quoteName(fkName)

  def dropForeignKeyStatement(foreignKeyTable: Table[_], fkName: String, session: AbstractSession): Unit =
    execFailSafeExecute(writeDropForeignKeyStatement(foreignKeyTable, fkName), e => true)

  def isTableDoesNotExistException(e: SQLException): Boolean

  def supportsForeignKeyConstraints = true

  def writeDropTable(tableName: String) =
    "drop table " + quoteName(tableName)

  def dropTable(t: Table[_]) =
    execFailSafeExecute(writeDropTable(t.prefixedName), e => isTableDoesNotExistException(e))

  def writeCompositePrimaryKeyConstraint(t: Table[_], cols: Iterable[FieldMetaData]) =
    writeUniquenessConstraint(t, cols);

  def writeUniquenessConstraint(t: Table[_], cols: Iterable[FieldMetaData]) = {
    // ALTER TABLE TEST ADD CONSTRAINT NAME_UNIQUE UNIQUE(NAME)
    val sb = new java.lang.StringBuilder(256)

    sb.append("alter table ")
    sb.append(quoteName(t.prefixedName))
    sb.append(" add constraint ")
    sb.append(quoteName(t.prefixedName + "CPK"))
    sb.append(" unique(")
    sb.append(cols.map(_.columnName).map(quoteName(_)).mkString(","))
    sb.append(")")
    sb.toString
  }

  def writeRegexExpression(left: ExpressionNode, pattern: String, sw: StatementWriter): Unit = {
    sw.write("(")
    left.write(sw)
    sw.write(" ~ ?)")
    sw.addParam(ConstantStatementParam(InternalFieldMapper.stringTEF.createConstant(pattern)))
  }

  def writeConcatOperator(left: ExpressionNode, right: ExpressionNode, sw: StatementWriter) = {
    val binaryOpNode = new BinaryOperatorNode(left, right, "||")
    binaryOpNode.doWrite(sw)
  }

//  /**
//   * @nameOfCompositeKey when not None, the column group forms a composite key, 'nameOfCompositeKey' can be used
//   * as part of the name to create a more meaningfull name for the constraint
//   */
//  def writeUniquenessConstraint(columnDefs: collection.Seq[FieldMetaData], nameOfCompositeKey: Option[String]) = ""

  /**
   * @param name the name specified in the Schema, when not None, it  must be used as the name
   * @param nameOfCompositeKey when not None, the column group forms a composite key, 'nameOfCompositeKey' can be used
   * as part of the name to create a more meaningfull name for the constraint, when 'name' is None
   */
  def writeIndexDeclaration(
    columnDefs: collection.Seq[FieldMetaData],
    name: Option[String],
    nameOfCompositeKey: Option[String],
    isUnique: Boolean
  ) = {
    val sb = new java.lang.StringBuilder(256)
    sb.append("create ")

    if (isUnique)
      sb.append("unique ")

    sb.append("index ")

    val tableName = columnDefs.head.parentMetaData.viewOrTable.prefixedName

    if (name.isDefined)
      sb.append(quoteName(name.get))
    else if (nameOfCompositeKey.isDefined)
      sb.append(quoteName("idx" + nameOfCompositeKey.get))
    else
      sb.append(
        quoteName(
          "idx" + generateAlmostUniqueSuffixWithHash(tableName + "-" + columnDefs.map(_.columnName).mkString("-"))
        )
      )

    sb.append(" on ")

    sb.append(quoteName(tableName))

    sb.append(columnDefs.map(_.columnName).map(quoteName(_)).mkString(" (", ",", ")"))

    sb.toString
  }

  /**
   * This will create an probabilistically unique string of length no longer than 11 chars,
   * it can be used to create "almost unique" names where uniqueness is not an absolute requirement,
   * is not ,
   */
  def generateAlmostUniqueSuffixWithHash(s: String): String = {
    val a32 = new java.util.zip.Adler32
    a32.update(s.getBytes)
    a32.getValue.toHexString
  }

  def quoteIdentifier(s: String) = s

  def quoteName(s: String) = s.split('.').map(quoteIdentifier(_)).mkString(".")

  def fieldAlias(n: QueryableExpressionNode, fse: FieldSelectElement) =
    n.alias + "_" + fse.fieldMetaData.columnName

  def aliasExport(parentOfTarget: QueryableExpressionNode, target: SelectElement) =
    parentOfTarget.alias + "_" + target.aliasSegment

  def writeSelectElementAlias(se: SelectElement, sw: StatementWriter) = {
    val a = se.aliasSegment
//    if(a.length > 30)
//      org.squeryl.internals.Utils.throwError("Oracle Bust : " + a)
    sw.write(quoteName(a))
  }

  def databaseTypeFor(fieldMapper: FieldMapper, c: Class[_]): String = {
    val ar = fieldMapper.sampleValueFor(c)
    val decl: String =
      if (ar.isInstanceOf[Enumeration#Value])
        intTypeDeclaration
      else if (classOf[String].isAssignableFrom(c))
        stringTypeDeclaration
      else if (ar.isInstanceOf[java.sql.Timestamp])
        timestampTypeDeclaration
      else if (ar.isInstanceOf[java.util.Date])
        dateTypeDeclaration
      else if (ar.isInstanceOf[java.lang.Integer])
        intTypeDeclaration
      else if (ar.isInstanceOf[java.lang.Long])
        longTypeDeclaration
      else if (ar.isInstanceOf[java.lang.Boolean])
        booleanTypeDeclaration
      else if (ar.isInstanceOf[java.lang.Double])
        doubleTypeDeclaration
      else if (ar.isInstanceOf[java.lang.Float])
        floatTypeDeclaration
      else if (ar.isInstanceOf[java.util.UUID])
        uuidTypeDeclaration
      else if (classOf[scala.Array[Byte]].isAssignableFrom(c))
        binaryTypeDeclaration
      else if (classOf[BigDecimal].isAssignableFrom(c))
        bigDecimalTypeDeclaration
      else if (classOf[scala.Array[Int]].isAssignableFrom(c))
        intArrayTypeDeclaration
      else if (classOf[scala.Array[Long]].isAssignableFrom(c))
        longArrayTypeDeclaration
      else if (classOf[scala.Array[Double]].isAssignableFrom(c))
        doubleArrayTypeDeclaration
      else if (classOf[scala.Array[String]].isAssignableFrom(c))
        stringArrayTypeDeclaration
      else
        Utils.throwError("unsupported type " + ar.getClass.getCanonicalName)

    decl
  }

  /*
  def writeCastInvocation(e: TypedExpression[_,_], sw: StatementWriter) = {
    sw.write("cast(")
    e.write(sw)

    val dbSpecificType = databaseTypeFor(e.mapper.jdbcClass)

    sw.write(" as ")
    sw.write(dbSpecificType)
    sw.write(")")
  }

  def writeCaseStatement(toMatch: Option[ExpressionNode], cases: Iterable[(ExpressionNode, TypedExpression[_,_])], otherwise: TypedExpression[_,_], sw: StatementWriter) = {

    sw.write("(case ")
    toMatch.foreach(_.write(sw))
    sw.indent
    sw.nextLine

    for(c <- cases) {
      sw.write("when ")
      c._1.write(sw)
      sw.write(" then ")
      writeCastInvocation(c._2, sw)
      sw.nextLine
    }

    sw.write("else ")
    writeCastInvocation(otherwise,sw)
    sw.nextLine
    sw.unindent
    sw.write("end)")
  }
   */

  def jdbcTypeConstantFor(c: Class[_]) =
    c.getCanonicalName match {
      case "java.lang.String" => Types.VARCHAR
      case "java.math.BigDecimal" => Types.DECIMAL
      case "java.lang.Boolean" => Types.BIT
      case "java.lang.Byte" => Types.TINYINT
      case "java.lang.Integer" => Types.INTEGER
      case "java.lang.Long" => Types.BIGINT
      case "java.lang.Float" => Types.FLOAT
      case "java.lang.Double" => Types.DOUBLE
      case "java.lang.Byte[]" => Types.BINARY
      case "byte[]" => Types.BINARY
      case "java.sql.Date" => Types.DATE
      case "java.util.Date" => Types.DATE
      case "java.sql.Timestamp" => Types.TIMESTAMP
      case "java.util.UUID" => Types.VARCHAR
      case "scala.math.BigDecimal" => Types.VARCHAR
      case s: Any =>
        throw new RuntimeException("Don't know jdbc type for " + s)
    }
}
