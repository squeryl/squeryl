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
 **************************************************************************** */
package org.squeryl


import dsl._
import ast._
import internals._
import reflect.{Manifest}
import java.sql.SQLException
import collection.mutable.{HashSet, ArrayBuffer}


trait Schema {

  protected implicit def thisSchema = this 

  private val _tables = new ArrayBuffer[Table[_]] 

  private val _oneToManyRelations = new ArrayBuffer[OneToManyRelation[_,_]]

  private val _manyToManyRelations = new ArrayBuffer[ManyToManyRelation[_,_,_]]

  private val _columnGroupAttributeAssignments = new ArrayBuffer[ColumnGroupAttributeAssignment]

  private [squeryl] val _namingScope = new HashSet[String] 

  private [squeryl] def _addRelation(r: OneToManyRelation[_,_]) =
    _oneToManyRelations.append(r)

  private [squeryl] def _addRelation(r: ManyToManyRelation[_,_,_]) =
    _manyToManyRelations.append(r)

  private def _dbAdapter = Session.currentSession.databaseAdapter

  /**
   * @returns a tuple of (Table[_], Table[_], ForeignKeyDeclaration) where
   *  ._1 is the foreign key table,
   *  ._2 is the primary key table
   *  ._3 is the ForeignKeyDeclaration between _1 and _2
   */
  private def _activeForeignKeySpecs = {
    val res = new ArrayBuffer[(Table[_], Table[_], ForeignKeyDeclaration)]

    for( r <- _oneToManyRelations if r.foreignKeyDeclaration._isActive)
      res.append((r.rightTable, r.leftTable, r.foreignKeyDeclaration))

    for(r <- _manyToManyRelations) {
      if(r.leftForeignKeyDeclaration._isActive)
        res.append((r.thisTable, r.leftTable , r.leftForeignKeyDeclaration))
      if(r.rightForeignKeyDeclaration._isActive)
        res.append((r.thisTable, r.rightTable, r.rightForeignKeyDeclaration))
    }

    res
  }

  @deprecated("will be removed in a future version")
  def findTableFor[A](a: A): Option[Table[A]] = {
    val c = a.asInstanceOf[AnyRef].getClass
    _tables.find(_.posoMetaData.clasz == c).asInstanceOf[Option[Table[A]]]
  }

  private def findAllTablesFor[A](c: Class[A]) =
    _tables.filter(t => c.isAssignableFrom(t.posoMetaData.clasz)).asInstanceOf[Traversable[Table[_]]]


  object NamingConventionTransforms {
    
    def camelCase2underScore(name: String) =
      name.toList.map(c => if(c.isUpper) "_" + c else c).mkString
  }

  def columnNameFromPropertyName(propertyName: String) = propertyName

  def tableNameFromClassName(tableName: String) = tableName

  def printDml = {

    for(t <- _tables) {
      val sw = new StatementWriter(true, _dbAdapter)
      _dbAdapter.writeCreateTable(t, sw, this)
    }
  }

  /**
   * This will drop all tables and related sequences in the schema... it's a
   * dangerous operation, typically this is only useful for development
   * database instances, the method is protected in order to make it a little
   * less 'accessible'
   */
  protected def drop: Unit = {

    if(_dbAdapter.supportsForeignKeyConstraints)
      _dropForeignKeyConstraints

    val s = Session.currentSession.connection.createStatement
    val con = Session.currentSession.connection

    for(t <- _tables) {
      _dbAdapter.dropTable(t)
      _dbAdapter.postDropTable(t)
    }
  }  

  def create = {
    _createTables
    if(_dbAdapter.supportsForeignKeyConstraints)
      _declareForeignKeyConstraints

    _createUniqueConstraintsOfCompositePKs

    createColumnGroupConstraintsAndIndexes
  }

  private def _writeColumnGroupAttributeAssignments: Seq[String] = {

    for(cgaa <- _columnGroupAttributeAssignments) yield {


      val unique = cgaa.findAttribute[Unique]
      val index = cgaa.findAttribute[Indexed]

      (unique, index) match {
        case (None,    None)                   => error("emtpy attribute list should not be possible to create with DSL (Squeryl bug).")
        case (Some(_), None)                   => _dbAdapter.writeIndexDeclaration(cgaa.columns, None,    cgaa.name, true)
        case (None,    Some(Indexed(idxName))) => _dbAdapter.writeIndexDeclaration(cgaa.columns, idxName, cgaa.name, false)
        case (Some(_), Some(Indexed(idxName))) => _dbAdapter.writeIndexDeclaration(cgaa.columns, idxName, cgaa.name, true)
      }
    }
  }

  def createColumnGroupConstraintsAndIndexes = {
      
    val cs = Session.currentSession

    for(statement <- _writeColumnGroupAttributeAssignments) {
            
      cs.log(statement)

      val s = cs.connection.createStatement
      try {
        s.execute(statement)
      }
      catch {
        case e:SQLException => throw new RuntimeException("error executing " + statement + "\n" + e, e)
      }
      finally {
        s.close
      }
    }
  }  

  private def _dropForeignKeyConstraints = {

    val cs = Session.currentSession
    val dba = cs.databaseAdapter

    for(fk <- _activeForeignKeySpecs) {
      val s = cs.connection.createStatement
      dba.dropForeignKeyStatement(fk._1, dba.foreignKeyConstraintName(fk._1, fk._3.idWithinSchema), cs)
    }
  }

  private def _declareForeignKeyConstraints =
    for(fk <- _activeForeignKeySpecs) {
      val fkDecl = fk._3

      val fkStatement = _dbAdapter.writeForeignKeyDeclaration(
         fk._1, fkDecl.foreignKeyColumnName,
         fk._2, fkDecl.referencedPrimaryKey,
         fkDecl._referentialAction1,
         fkDecl._referentialAction2,
         fkDecl.idWithinSchema
      )
            
      val cs = Session.currentSession
      val s = cs.connection.createStatement
      try {
        s.execute(fkStatement)
      }
      catch {
        case e:SQLException => throw new RuntimeException("error executing " + fkStatement + "\n" + e, e)
      }
      finally {
        s.close
      }
    }


  private def _createTables =
    for(t <- _tables) {
      var sw:StatementWriter = null
      try {
        sw = new StatementWriter(_dbAdapter)
        _dbAdapter.writeCreateTable(t, sw, this)
        val cs = Session.currentSession
        val s = cs.connection.createStatement
        val createS = sw.statement
        if(cs.isLoggingEnabled)
          cs.log(createS)
        s.execute(createS)
      }
      catch {
        case e:SQLException => throw new RuntimeException(
          "error creating table : " +
          e.getMessage + "ErrorCode:" + e.getErrorCode + ", SQLState:" + e.getSQLState + "\n" + sw.statement, e
        )
      }

      _dbAdapter.postCreateTable(Session.currentSession, t)
    }    

  private def _createUniqueConstraintsOfCompositePKs = {

    val cs = Session.currentSession

    for(cpk <- _allCompositePrimaryKeys) {

      val createConstraintStmt = _dbAdapter.writeUniquenessConstraint(cpk._1, cpk._2)
      val s = cs.connection.createStatement
      try{
        if(cs.isLoggingEnabled)
          cs.log(createConstraintStmt)
        s.execute(createConstraintStmt)
      }
      finally {
        s.close
      }
    }
  }

  /**
   * returns an Iterable of (Table[_],Iterable[FieldMetaData]), the list of
   * all tables whose PK is a composite, with the columns that are part of the PK : Iterable[FieldMetaData] 
   */
  private def _allCompositePrimaryKeys = {
    
    val res = new ArrayBuffer[(Table[_],Iterable[FieldMetaData])]
    
    for(t <- _tables
        if classOf[KeyedEntity[_]].isAssignableFrom(t.posoMetaData.clasz)) {

      Utils.mapSampleObject(
        t.asInstanceOf[Table[KeyedEntity[_]]],
        (ke:KeyedEntity[_]) => {
          val id = ke.id
          if(id.isInstanceOf[CompositeKey]) {
            val compositeCols = id.asInstanceOf[CompositeKey]._fields
            res.append((t, compositeCols))
          }
        }
      )
    }

    res
  }

  protected def columnTypeFor(fieldMetaData: FieldMetaData, databaseAdapter: DatabaseAdapter): String =
    databaseAdapter.databaseTypeFor(fieldMetaData)

  private [squeryl] def _columnTypeFor(fmd: FieldMetaData, dba: DatabaseAdapter): String =
    this.columnTypeFor(fmd, dba)
  
  def tableNameFromClass(c: Class[_]):String =     
    c.getSimpleName

  protected def table[T]()(implicit manifestT: Manifest[T]): Table[T] =
    table(tableNameFromClass(manifestT.erasure))(manifestT)
  
  protected def table[T](name: String)(implicit manifestT: Manifest[T]): Table[T] = {
    val t = new Table[T](name, manifestT.erasure.asInstanceOf[Class[T]], this)
    _addTable(t)
    t
  }

  private [squeryl] def _addTable(t:Table[_]) =
    _tables.append(t)
  
  protected def view[T]()(implicit manifestT: Manifest[T]): View[T] =
    view(tableNameFromClass(manifestT.erasure))(manifestT)

  protected def view[T](name: String)(implicit manifestT: Manifest[T]): View[T] =
    new View[T](name)(manifestT)

  
  class ReferentialEvent(val eventName: String) {
    def restrict = new ReferentialActionImpl("restrict", this)
    def cascade = new ReferentialActionImpl("cascade", this)
    def noAction = new ReferentialActionImpl("no action", this)
  }

  class ReferentialActionImpl(token: String, ev: ReferentialEvent) extends ReferentialAction {
    def event = ev.eventName
    def action = token
  }
  
  protected def onUpdate = new ReferentialEvent("update")

  protected def onDelete = new ReferentialEvent("delete")

  private var _fkIdGen = 1 

  private [squeryl] def _createForeignKeyDeclaration(fkColName: String, pkColName: String) = {
    val fkd = new ForeignKeyDeclaration(_fkIdGen, fkColName, pkColName)
    _fkIdGen += 1
    applyDefaultForeignKeyPolicy(fkd)
    fkd
  }

  def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) =
    foreignKeyDeclaration.constrainReference

  @deprecated("Use applyDefaultForeignKeyPolicy instead")
  final def applyDefaultForeingKeyPolicy(foreingKeyDeclaration: ForeignKeyDeclaration) =
    applyDefaultForeignKeyPolicy(foreingKeyDeclaration)

  /**
   * @return a Tuple2 with (LengthOfDecimal, Scale) that will determine the storage
   * length of the database type that map fields of type java.lang.BigDecimal
   * Can be overridden by the Column Annotation, ex.: Column(length=22, scale=20)
   * default is (20,16)
   */
  
  def defaultSizeOfBigDecimal = (20,16)

  /**
   * @return the default database storage (column) length for String columns for this Schema,
   * Can be overridden by the Column Annotation ex.: Column(length=256)
   * default is 128 
   */
  def defaultLengthOfString = 123

  /**
   * protected since table declarations must only be done inside a Schema
   */

  protected def declare[B](a: BaseColumnAttributeAssignment*) = a

  /**
   * protected since table declarations must only be done inside a Schema
   */  
  protected def on[A](table: Table[A]) (declarations: A=>Seq[BaseColumnAttributeAssignment]) = {

    val colAss: Seq[BaseColumnAttributeAssignment] =
      Utils.mapSampleObject(table, declarations)

    // all fields that have declarations are first reset :
    for(ca <- colAss)
      ca.clearColumnAttributes

    for(ca <- colAss) ca match {
      case dva:DefaultValueAssignment    => dva.left._defaultValue = Some(dva.value)
      case caa:ColumnAttributeAssignment => {

        for(ca <- caa.columnAttributes)
          (caa.left._addColumnAttribute(ca))
      }
      case ctaa:ColumnGroupAttributeAssignment => _addClumnGroupAttributeAssignment(ctaa)
      case a:Any => error("did not match on " + a.getClass.getName)
    }

    // Validate that a KeyedEntity.id is not left without a uniqueness constraint :
    for(ca <- colAss.find(_.isIdFieldOfKeyedEntity))
      assert(
        ca.columnAttributes.exists(_.isInstanceOf[PrimaryKey]) ||
        ca.columnAttributes.exists(_.isInstanceOf[Unique]),
        "Column 'id' of table '" + table.name +
        "' must have a uniqueness constraint by having the column attribute 'primaryKey' or 'unique' to honor it's KeyedEntity trait"
      )

    // Validate that autoIncremented is not used on other fields than KeyedEntity[A].id :
    // since it is not yet unsupported :
    for(ca <- colAss) ca match {
      case cga:CompositeKeyAttributeAssignment => {}
      case caa:ColumnAttributeAssignment => {
        for(ca <- caa.columnAttributes if ca.isInstanceOf[AutoIncremented] && !(caa.left.isIdFieldOfKeyedEntity))
          error("Field " + caa.left.nameOfProperty + " of table " + table.name +
                " is declared as autoIncrementeded, auto increment is currently only supported on KeyedEntity[A].id")
      }
      case dva:Any => {}
    }
  }

  private def _addClumnGroupAttributeAssignment(cga: ColumnGroupAttributeAssignment) =
    _columnGroupAttributeAssignments.append(cga);
  
  def defaultColumnAttributesForKeyedEntityId = Set(new PrimaryKey, new AutoIncremented(None))
  
  protected def unique = Unique()

  protected def primaryKey = PrimaryKey()

  protected def autoIncremented = AutoIncremented(None)
  
  protected def autoIncremented(sequenceName: String) = AutoIncremented(Some(sequenceName))

  protected def indexed = Indexed(None)

  protected def indexed(indexName: String) = Indexed(Some(indexName))

  class ColGroupDeclaration(cols: Seq[FieldMetaData]) {

    def are(columnAttributes: AttributeValidOnMultipleColumn*) =
      new ColumnGroupAttributeAssignment(cols, columnAttributes)
  }

  def columns(fieldList: TypedExpressionNode[_]*) = new ColGroupDeclaration(fieldList.map(_._fieldMetaData))
}
