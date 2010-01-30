package org.squeryl.adapters

import org.squeryl.internals.{StatementWriter, DatabaseAdapter}
import org.squeryl.{Session, Table}
import org.squeryl.dsl.ast.{QueryExpressionElements, SelectElementReference, QueryableExpressionNode, FunctionNode}

class OracleAdapter extends DatabaseAdapter {

  override def intTypeDeclaration = "number"
  override def stringTypeDeclaration = "varchar2(255)"
  override def stringTypeDeclaration(length:Int) = "varchar2("+length+")"
  override def booleanTypeDeclaration = "number(1)"
  override def doubleTypeDeclaration = "double precision"
  override def longTypeDeclaration = "number"

  override def areAutoIncrementFieldsSupported: Boolean = false

  override def postCreateTable(s: Session, t: Table[_]) = {

    val sw = new StatementWriter(false, this)
    //TODO: set max value from t.posoMetaData.primaryKey
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
    sw.write(colNames.map(fmd => fmd.name).mkString(", "));
    sw.write(") values ");
    sw.write(colVals.mkString("(",",",")"));
  }

  override def writeConcatFunctionCall(fn: FunctionNode, sw: StatementWriter) =
    sw.writeNodesWithSeparator(fn.args, " || ", false)

  override def writeOuterJoin(qen: QueryableExpressionNode, sw: StatementWriter, left: SelectElementReference, right: SelectElementReference, outerJoinKind: String) = {
    sw.write(outerJoinKind)
    sw.write(" outer join ")
    qen.write(sw)
    sw.write(" ")
    sw.write(qen.alias)
    sw.write(" on ")
    left.write(sw)
    sw.write(" = ")
    right.write(sw)
  }

}