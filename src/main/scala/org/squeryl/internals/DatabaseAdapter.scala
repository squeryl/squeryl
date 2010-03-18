package org.squeryl.internals

import org.squeryl.dsl.ast._
import java.sql.{ResultSet, SQLException, PreparedStatement, Connection}
import org.squeryl.{Schema, Session, Table}
import org.squeryl.customtypes.CustomType

class DatabaseAdapter {

  class Zip[T](val element: T, val isNotLast: Boolean)
  
  class ZipIterable[T](iterable: Iterable[T]) {
    val count = iterable.size
    def foreach[U](f: Zip[T] => U):Unit = {
      var c = 1  
      for(i <- iterable) {
        f(new Zip(i, c < count))
        c += 1
      }
    }

    def zipi = this
  }

  implicit def zipIterable[T](i: Iterable[T]) = new ZipIterable(i)

  def writeQuery(qen: QueryExpressionElements, sw: StatementWriter):Unit = {

    sw.write("Select")

    if(qen.selectDistinct)
      sw.write(" distinct")
    
    sw.nextLine
    sw.writeIndented {
      sw.writeNodesWithSeparator(qen.selectList.filter(e => ! e.inhibited), ",", true)
    }
    sw.nextLine
    sw.write("From")
    sw.nextLine
    var fromListSize = 0
    sw.writeIndented {
      for(z <- qen.tableExpressions.zipi) {
        fromListSize += 1
        z.element.write(sw)
        sw.write(" ")
        sw.write(z.element.alias)
        if(z.isNotLast) {
          sw.write(",")
          sw.nextLine
        }
      }      
    }
    
    sw.pushPendingNextLine
    
    if(qen.outerJoinExpressions != Nil) sw.writeIndented {
      for(oje <- qen.outerJoinExpressions.zipi) {
        writeOuterJoin(oje.element, sw)
        sw.pushPendingNextLine
      }
    }
    
    if(qen.whereClause != None && qen.whereClause.get.children.filter(c => !c.inhibited) != Nil) {      
      sw.write("Where")
      sw.nextLine
      sw.writeIndented {
        qen.whereClause.get.write(sw)
      }
      sw.pushPendingNextLine
    }

    if(! qen.groupByClause.isEmpty) {      
      sw.write("Group By")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.groupByClause.filter(e => ! e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }
    
    if(! qen.orderByClause.isEmpty) {
      sw.write("Order By")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.orderByClause.filter(e => ! e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }

    if(qen.isForUpdate) {
      sw.write("for update")
      sw.pushPendingNextLine
    }

    writePaginatedQueryDeclaration(qen, sw)
  }

  def writePaginatedQueryDeclaration(qen: QueryExpressionElements, sw: StatementWriter):Unit = 
    qen.page.foreach(p => {
      sw.write("limit ")
      sw.write(p._2.toString)
      sw.write(" offset ")
      sw.write(p._1.toString)
    })

  
  def writeOuterJoin(oje: OuterJoinExpression, sw: StatementWriter) = {
    sw.write(oje.leftRightOrFull)
    sw.write(" outer join ")
    oje.queryableExpressionNode.write(sw)
    sw.write(" as ")
    sw.write(oje.queryableExpressionNode.alias)
    sw.write(" on ")
    oje.matchExpression.write(sw)
  }

  def intTypeDeclaration = "int"
  def stringTypeDeclaration = "varchar(255)"  
  def stringTypeDeclaration(length:Int) = "varchar("+length+")"
  def booleanTypeDeclaration = "boolean"
  def doubleTypeDeclaration = "double"
  def dateTypeDeclaration = "date"
  def longTypeDeclaration = "bigint"
  def floatTypeDeclaration = "real"
  
  private val _declarationHandler = new FieldTypeHandler[String] {

    def handleIntType = intTypeDeclaration
    def handleStringType  = stringTypeDeclaration
    def handleBooleanType = booleanTypeDeclaration
    def handleDoubleType = doubleTypeDeclaration
    def handleDateType = dateTypeDeclaration
    def handleLongType = longTypeDeclaration
    def handleFloatType = floatTypeDeclaration
    def handleUnknownType(c: Class[_]) =
      error("don't know how to map field type " + c.getName)
  }
  
  def databaseTypeFor(fmd: FieldMetaData) =
    _declarationHandler.handleType(fmd.wrappedFieldType)

  def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    val dbTypeDeclaration = schema._columnTypeFor(fmd, this)

    var res = "  " + fmd.columnName + " " + dbTypeDeclaration

    if(isPrimaryKey)
      res += " primary key"

    if(!fmd.isOption)
      res += " not null"
    
    if(supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      res += " auto_increment"

    res
  }

  def supportsAutoIncrementInColumnDeclaration:Boolean = true

  def writeCreateTable[T](t: Table[T], sw: StatementWriter, schema: Schema) = {

    sw.write("create table ")
    sw.write(t.name);
    sw.write(" (\n");
    val pk = t.posoMetaData.primaryKey;    
    sw.writeIndented {
      sw.writeLinesWithSeparator(
        t.posoMetaData.fieldsMetaData.map(
          fmd => writeColumnDeclaration(fmd, pk != None && pk.get == fmd, schema)
        ),
        ","
      )
    }
    sw.write(")\n ")
  }

  def prepareStatement(c: Connection, sw: StatementWriter, session: Session): PreparedStatement =
    prepareStatement(c, sw, c.prepareStatement(sw.statement), session)
  
  def prepareStatement(c: Connection, sw: StatementWriter, s: PreparedStatement, session: Session): PreparedStatement = {    

    session._addStatement(s)

    var i = 1;
    for(p <- sw.params) {

      if(p.isInstanceOf[Option[_]]) {
        val o = p.asInstanceOf[Option[_]]
        if(o == None)
          s.setObject(i, null)
        else
          s.setObject(i, o.get)
      }
      else
        s.setObject(i, p)
      i += 1
    }
    s
  }

  private def _exec[A](s: Session, sw: StatementWriter)(block: =>A) =
    try {
      if(s.isLoggingEnabled)
        s.log(sw.toString)      
      block
    }
    catch {
      case e: SQLException => throw new RuntimeException("Exception while executing statement :\n" + sw.statement, e)
    }

  def executeQuery(s: Session, sw: StatementWriter) = _exec(s, sw) {
    val st = prepareStatement(s.connection, sw, s)
    (st.executeQuery, st)
  }

  def executeUpdate(s: Session, sw: StatementWriter):(Int,PreparedStatement) = _exec(s, sw) {
    val st = prepareStatement(s.connection, sw, s)
    (st.executeUpdate, st)
  }

  def executeUpdateForInsert(s: Session, sw: StatementWriter, ps: PreparedStatement) = _exec(s, sw) {
    val st = prepareStatement(s.connection, sw, ps, s)
    (st.executeUpdate, st)
  }

  def writeInsert[T](o: T, t: Table[T], sw: StatementWriter):Unit = {

    val o_ = o.asInstanceOf[AnyRef]    
    val f = t.posoMetaData.fieldsMetaData.filter(fmd => !fmd.isAutoIncremented)

    sw.write("insert into ");
    sw.write(t.name);
    sw.write(" (");
    sw.write(f.map(fmd => fmd.columnName).mkString(", "));
    sw.write(") values ");
    sw.write(
      f.map(fmd => writeValue(o_, fmd, sw)
    ).mkString("(",",",")"));
  }

  /**
   * Converts field instances so they can be fed, and understood by JDBC
   * will not do conversion from None/Some, so @arg r should be a java primitive type or
   * a CustomType
   */
  def convertToJdbcValue(r: AnyRef) : AnyRef = {
    var v = r
    if(v.isInstanceOf[CustomType])
       v = v.asInstanceOf[CustomType].wrappedValue.asInstanceOf[AnyRef]
    if(v.isInstanceOf[java.util.Date] && ! v.isInstanceOf[java.sql.Date])
       v = new java.sql.Date(v.asInstanceOf[java.util.Date].getTime)

//  see comment in def convertFromBooleanForJdbc    
//    if(v.isInstanceOf[java.lang.Boolean])
//      v = convertFromBooleanForJdbc(v)
    
    v
  }

  // TODO: move to StatementWriter (since it encapsulates the 'magic' of swapping values for '?' when needed)
  //and consider delaying the ? to 'value' decision until execution, in order to make StatementWriter loggable
  //with values at any time (via : a kind of prettyStatement method)
  protected def writeValue(o: AnyRef, fmd: FieldMetaData, sw: StatementWriter):String =
    if(sw.isForDisplay) {
      val v = fmd.get(o)
      if(v != null)
        v.toString
      else
        "null"
    }
    else {
      sw.addParam(convertToJdbcValue(fmd.get(o)))
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

  def postCreateTable(s: Session, t: Table[_]) = {}
  
  def postDropTable(t: Table[_]) = {}

  def writeConcatFunctionCall(fn: FunctionNode[_], sw: StatementWriter) = {
    sw.write(fn.name)
    sw.write("(")
    sw.writeNodesWithSeparator(fn.args, ",", false)
    sw.write(")")    
  }

  def isFullOuterJoinSupported = true

  def writeUpdate[T](o: T, t: Table[T], sw: StatementWriter, checkOCC: Boolean) = {

    val o_ = o.asInstanceOf[AnyRef]
    val pkMd = t.posoMetaData.primaryKey.get

    sw.write("update ", t.name, " set ")
    sw.nextLine
    sw.indent
    sw.writeLinesWithSeparator(
      t.posoMetaData.fieldsMetaData.
        filter(fmd=> fmd != pkMd).
          map(fmd => {
            if(fmd.isOptimisticCounter)
              fmd.columnName + " = " + fmd.columnName + " + 1 "
            else
              fmd.columnName + " = " + writeValue(o_, fmd, sw)
          }),
      ","
    )
    sw.unindent
    sw.write("where")
    sw.nextLine
    sw.indent
    sw.write(pkMd.columnName, " = ", writeValue(o_, pkMd, sw))

    if(checkOCC)
      t.posoMetaData.optimisticCounter.foreach(occ => {
         sw.write(" and ")
         sw.write(occ.columnName)
         sw.write(" = ")
         sw.write(writeValue(o_, occ, sw))
      })
  }

  def writeDelete[T](t: Table[T], whereClause: Option[ExpressionNode], sw: StatementWriter) = {

    sw.write("delete from ")
    sw.write(t.name)
    if(whereClause != None) {
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
  def convertToBooleanForJdbc(rs: ResultSet, i:Int): Boolean = rs.getBoolean(i)


  def writeUpdate(t: Table[_], us: UpdateStatement, sw : StatementWriter) = {

    val colsToUpdate = us.columns.iterator

    sw.write("update ")
    sw.write(t.name)
    sw.write(" set")
    sw.indent
    sw.nextLine
    for(z <- us.values.zipi) {
      val col = colsToUpdate.next
      sw.write(col.columnName)
      sw.write(" = ")
      val v = z.element
      v.write(sw)
      if(z.isNotLast) {
        sw.write(",")
        sw.nextLine
      }      
    }

    if(t.posoMetaData.isOptimistic) {
      sw.write(",")
      sw.nextLine      
      val occ = t.posoMetaData.optimisticCounter.get
      sw.write(occ.columnName)
      sw.write(" = ")
      sw.write(occ.columnName + " + 1")
    }
    
    sw.unindent

    if(us.whereClause != None) {
      sw.nextLine
      sw.write("Where")
      sw.nextLine
      val whereClauseClosure = us.whereClause.get
      sw.writeIndented {
        whereClauseClosure().write(sw)
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
}