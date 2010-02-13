package org.squeryl.adapters

import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.{Session, Table}
import org.squeryl.dsl.ast.FunctionNode
import java.sql.ResultSet

class PostgreSqlAdapter extends DatabaseAdapter {

  override def intTypeDeclaration = "integer"
  override def stringTypeDeclaration = "varchar(255)"
  override def stringTypeDeclaration(length:Int) = "varchar("+length+")"
  override def booleanTypeDeclaration = "boolean"
  override def doubleTypeDeclaration = "double precision"
  override def longTypeDeclaration = "bigint"

  override def postCreateTable(s: Session, t: Table[_]) = {

    val sw = new StatementWriter(false, this)
    //TODO: set max value from t.posoMetaData.primaryKey
    sw.write("create sequence ", sequenceName(t))
    val st = s.connection.createStatement
    st.execute(sw.statement)
  }

  override def postDropTable(t: Table[_]) = {
    val sw = new StatementWriter(false, this)
    sw.write("drop sequence ", sequenceName(t))
    val st = Session.currentSession.connection.createStatement
    st.execute(sw.statement)
  }

  def sequenceName(t: Table[_]) = "seq_" + t.name

  override def writeConcatFunctionCall(fn: FunctionNode[_], sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " || ", false)
  
  override def writeInsert[T](o: T, t: Table[T], sw: StatementWriter):Unit = {

    val o_ = o.asInstanceOf[AnyRef]

    val autoIncPK = t.posoMetaData.fieldsMetaData.find(fmd => fmd.isAutoIncremented)

    if(autoIncPK == None) {
      super.writeInsert(o, t, sw)
      return
    }

    val f = t.posoMetaData.fieldsMetaData.filter(fmd => fmd != autoIncPK.get)

    val colNames = List(autoIncPK.get) ::: f.toList
    val colVals = List("nextval('" + sequenceName(t) + "')") ::: f.map(fmd => writeValue(o_, fmd, sw)).toList

    sw.write("insert into ");
    sw.write(t.name);
    sw.write(" (");
    sw.write(colNames.map(fmd => fmd.columnName).mkString(", "));
    sw.write(") values ");
    sw.write(colVals.mkString("(",",",")"));
  }

  override def supportsAutoIncrementInColumnDeclaration: Boolean = false
}