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

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method, Constructor, InvocationTargetException, Type, ParameterizedType}
import java.sql.ResultSet
import java.math.BigDecimal
import scala.annotation.tailrec
import org.squeryl.annotations.{ColumnBase, Column}
import collection.mutable.{HashMap, HashSet, ArrayBuffer}
import org.squeryl.Session
import org.squeryl.dsl.CompositeKey
import org.squeryl.customtypes.CustomType
import scala.tools.scalap.scalax.rules.scalasig.{ScalaSigAttributeParsers, ByteCode, ScalaSigPrinter}
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.lang.reflect.Member
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigParser
import org.squeryl.dsl.ast.ConstantTypedExpression
import org.squeryl.customtypes.CustomType

class FieldMetaData(
        val parentMetaData: PosoMetaData[_],
        val nameOfProperty:String,
        val fieldType: Class[_], // if isOption, this fieldType is the type param of Option, i.e. the T in Option[T]
        val wrappedFieldType: Class[_], //in primitive type mode fieldType == wrappedFieldType, in custom type mode wrappedFieldType is the 'real'
        // type, i.e. the (primitive) type that jdbc understands
        val customTypeFactory: Option[AnyRef => Product1[Any] with AnyRef],
        val isOption: Boolean,
        getter: Option[Method],
        setter: Option[Method],
        field:  Option[Field],
        columnAnnotation: Option[Column],
        val isOptimisticCounter: Boolean,
        val sampleValue: Any) {

  def nativeJdbcType =
    this.schema.fieldMapper.nativeJdbcTypeFor(wrappedFieldType)

  /**
   * None if this FieldMetaData is not an enumeration, Some(theParentEnumeration) otherwise
   */
  val enumeration: Option[Enumeration] =
    sampleValue match {
      case Some(e: Enumeration#Value) => Some(Utils.enumerationForValue(e))
      case e: Enumeration#Value => Some(Utils.enumerationForValue(e))
      case _ => None
    }


  def canonicalEnumerationValueFor(id: Int) =
    if(sampleValue == null)
      org.squeryl.internals.Utils.throwError("classes with Enumerations must have a zero param constructor that assigns a sample to the enumeration field")
    else
      enumeration flatMap { e: Enumeration =>
        e.values find { _.id == id }
      } get

  /**
   * This field is mutable only by the Schema trait, and only during the Schema instantiation,
   * so it can safely be considered immutable (read only) by the columnAttributes accessor 
   */
  private val _columnAttributes = new HashSet[ColumnAttribute]


  private [squeryl] def _clearColumnAttributes = {
    _columnAttributes.clear
  }

  private [squeryl] def _addColumnAttribute(ca: ColumnAttribute) =
      _columnAttributes.add(ca)

  /**
   * In some circumstances (like in the test suite) a Schema instance must run on multiple database types,
   * this Map keeps the sequence names 'per schema'
   */
  private val _sequenceNamePerDBAdapter = new HashMap[Class[_],String]

  def sequenceName: String = {

    val ai = _columnAttributes.find(_.isInstanceOf[AutoIncremented]).
      getOrElse(org.squeryl.internals.Utils.throwError(this + " is not declared as autoIncremented, hence it has no sequenceName")).
        asInstanceOf[AutoIncremented]

    if(ai.nameOfSequence != None) {
      return ai.nameOfSequence.get
    }

    synchronized {
      val c = Session.currentSession.databaseAdapter.getClass

      val s = _sequenceNamePerDBAdapter.get(c)

      if(s != None)
        return s.get

      val s0 = Session.currentSession.databaseAdapter.createSequenceName(this)
      
      _sequenceNamePerDBAdapter.put(c, s0)

      return s0
    }
  }

  def isIdFieldOfKeyedEntity =
    parentMetaData.viewOrTable.ked.map(_.idPropertyName == nameOfProperty).getOrElse(false)

  if(isIdFieldOfKeyedEntity && ! classOf[CompositeKey].isAssignableFrom(wrappedFieldType)) {
    schema.defaultColumnAttributesForKeyedEntityId(wrappedFieldType).foreach(ca => {

      if(ca.isInstanceOf[AutoIncremented] && ! (wrappedFieldType.isAssignableFrom(classOf[java.lang.Long]) || wrappedFieldType.isAssignableFrom(classOf[java.lang.Integer])))
        org.squeryl.internals.Utils.throwError("Schema " + schema.getClass.getName + " has method defaultColumnAttributesForKeyedEntityId returning AutoIncremented \nfor " +
          " all KeyedEntity tables, while class " + parentMetaData.clasz.getName +
          "\n has it's id field of type " + fieldType.getName + ", that is neither an Int or a Long, \n the only two types that can " +
          "be auto incremented")

      _addColumnAttribute(ca)
    })
  }

  private [squeryl] var _defaultValue: Option[ConstantTypedExpression[_,_]] = None

  def columnAttributes: Iterable[ColumnAttribute] = _columnAttributes

  def defaultValue: Option[ConstantTypedExpression[_,_]] = _defaultValue

  /**
   * The db column type declaration overriden in the schema, if None, it means that it is the default value for
   * the adapter (see Correspondance of field types to database column types http://squeryl.org/schema-definition.html)  
   */
  def explicitDbTypeDeclaration: Option[String] = {
    val dbt = _columnAttributes.find(_.isInstanceOf[DBType])
    if(dbt == None)
      None
    else
      Some(dbt.get.asInstanceOf[DBType].declaration)
  }
  
  def isTransient =
    _columnAttributes.exists(_.isInstanceOf[IsTransient])

  def isCustomType = customTypeFactory != None

  /**
   * @return the length defined in org.squeryl.annotations.Column.length
   * if it is defined, or the default length for Java primitive types.
   * The unit of the length is dependent on the type, the convention is
   * that numeric types have a length in byte, boolean is bits
   * date has -1, and for string the lenght is in chars.  
   * double,long -> 8, float,int -> 4, byte -> 1, boolean -> 1
   * java.util.Date -> -1.
   *
   * The use of this field is to help custom schema generators select
   * the most appropriate column type  
   */
  def length: Int =
    if(columnAnnotation == None || columnAnnotation.get.length == -1) {
      FieldMetaData.defaultFieldLength(wrappedFieldType, this)
    }
    else
      columnAnnotation.get.length

  def scale: Int =
    if(columnAnnotation == None || columnAnnotation.get.scale == -1)
      schema.defaultSizeOfBigDecimal._2
    else
      columnAnnotation.get.scale   

  def schema = parentMetaData.schema

  /**
   * The name of the database column
   */
  def columnName =
    if(columnAnnotation == None) {
      val nameDefinedInSchema = _columnAttributes.find(_.isInstanceOf[Named]).map(_.asInstanceOf[Named].name)      
      parentMetaData.schema.columnNameFromPropertyName(nameDefinedInSchema.getOrElse(nameOfProperty))
    }
    else {
      val ca = columnAnnotation.get
      var res = ca.name

      if(res == "")
        res = ca.value

      if(res == "")
        parentMetaData.schema.columnNameFromPropertyName(nameOfProperty)
      else
        res
    }
  
  protected def createResultSetHandler =
    this.schema.fieldMapper.resultSetHandlerFor(wrappedFieldType)

  val resultSetHandler = createResultSetHandler
    
  if(!isCustomType)
    assert(fieldType == wrappedFieldType,
      "expected fieldType == wrappedFieldType in primitive type mode, got "+
      fieldType.getName + " != " + wrappedFieldType.getName)

  override def toString =
    parentMetaData.clasz.getSimpleName + "." + columnName + ":" + displayType

  def isStringType =
    wrappedFieldType.isAssignableFrom(classOf[String])  

  def displayType =
     (if(isOption)
        "Option[" + fieldType.getName + "]"
      else
        fieldType.getName)

  /**
   * When true, will cause Schema generation to declare as PrimaryKey, Note that for
   * KeyedEntity[]s,  declaredAsPrimaryKeyInSchema is always true, and the cannot be made otherwise,
   * the inverse is not true, a field can be declared as primary key in the Shema without it being the
   * id of a KeyedEntity[], ex. :
   *  
   * <pre>
   * on(myTable)(t =>declare(
   *   myField.is(primaryKey)  // myField doesn't need to be a KeyedEntity.id 
   * ))
   * </pre>
   * 
   * <pre>
   * on(myKeyedEntityTable)(t =>declare(
   *   id.is(autoIncremented)  // omiting primaryKey here has no effect, it is equivalent as id.is(primaryKey,autoIncremented)
   * ))
   * </pre>
   */
  def declaredAsPrimaryKeyInSchema =
    columnAttributes.exists(_.isInstanceOf[PrimaryKey])

  def isAutoIncremented =
    columnAttributes.exists(_.isInstanceOf[AutoIncremented])

  /**
   * Inserts will only set values for a column if isInsertable is true
   */
  def isInsertable =
    !columnAttributes.exists(_.isInstanceOf[Uninsertable])

  /**
   * Updates will only set values for a column if isUpdatable is true
   */
  def isUpdatable =
    !columnAttributes.exists(_.isInstanceOf[Unupdatable])

  /**
   *  gets the value of the field from the object.
   * Note that it will unwrap Option[] and return null instead of None, i.e.
   * if converts None and Some to null and some.get respectively 
   * @arg o the object that owns the field
   */
  def get(o:AnyRef): AnyRef =
    try {
      val res =
        if(getter != None)
          _getFromGetter(o)
        else
          _getFromField(o)

      if(isOption) {
        if(res == None)
          null
        else
          res.asInstanceOf[Option[_]].get.asInstanceOf[AnyRef]
      }
      else
        res
    }
    catch {
      case e: IllegalArgumentException => org.squeryl.internals.Utils.throwError(wrappedFieldType.getName + " used on " + o.getClass.getName)
    }
    
  def getNativeJdbcValue(o:AnyRef): AnyRef = {
    val r = get(o)
    schema.fieldMapper.nativeJdbcValueFor(wrappedFieldType, r)
  }

  def setFromResultSet(target: AnyRef, rs: ResultSet, index: Int) = {
    val v = resultSetHandler(rs, index)
    set(target, v)    
  }
  
  /**
   * Sets the value 'v' to the object, the value will be converted to Some or None
   * if the field is an Option[], (if isOption).   
   */
  def set(target: AnyRef, v: AnyRef) = {
    try {
      val v0: AnyRef =
        if(v == null)
          null
        else if(enumeration != None)
          canonicalEnumerationValueFor(v.asInstanceOf[java.lang.Integer].intValue)
        else if(customTypeFactory == None)
          v
        else {
          val f = customTypeFactory.get

          if(v.isInstanceOf[CustomType[_]]) {
            val r = v.asInstanceOf[CustomType[_]]._1
            f(if(r == null) null else r.asInstanceOf[AnyRef])
          }          
          else {
           f(v)
          }
        }

      val actualValue =
        if(!isOption)
          v0
        else if(v0 == null)
          None
        else
          Some(v0)

      val res =
        if(setter != None)
          _setWithSetter(target, actualValue)
        else
          _setWithField(target, actualValue)
    }
    catch {
      case e: Exception => {
        val typeOfV = if(v == null) "null" else v.getClass.getCanonicalName
        org.squeryl.internals.Utils.throwError(
          this + " was invoked with value '" + v + "' of type " + typeOfV + " on object of type " + target.getClass.getName + " \n" + e)
      }
    }

  }

  private def _getFromGetter(o:AnyRef) =
    getter.get.invoke(o)

  private def _setWithSetter(target: AnyRef, v: AnyRef) =
    setter.get.invoke(target, v)

  private def _getFromField(o:AnyRef) =
    field.get.get(o)

  private def _setWithField(target: AnyRef, v: AnyRef) =
    field.get.set(target, v)
}

trait FieldMetaDataFactory {

  def hideFromYieldInspection(o: AnyRef, f: Field): Boolean = false

  def build(parentMetaData: PosoMetaData[_], name: String, property: (Option[Field], Option[Method], Option[Method], Set[Annotation]), sampleInstance4OptionTypeDeduction: AnyRef, isOptimisticCounter: Boolean): FieldMetaData

  def createPosoFactory(posoMetaData: PosoMetaData[_]): ()=>AnyRef
}

object FieldMetaData {

  private val _EMPTY_ARRAY = new Array[Object](0)
  
  var factory = new FieldMetaDataFactory {   

    def createPosoFactory(posoMetaData: PosoMetaData[_]): ()=>AnyRef =
      () => {
        val c = posoMetaData.constructor
        c._1.newInstance(c._2 :_*).asInstanceOf[AnyRef];
      }

    def build(parentMetaData: PosoMetaData[_], name: String, property: (Option[Field], Option[Method], Option[Method], Set[Annotation]), sampleInstance4OptionTypeDeduction: AnyRef, isOptimisticCounter: Boolean) = {

      val fieldMapper = parentMetaData.schema.fieldMapper 
      
      val field  = property._1
      val getter = property._2
      val setter = property._3
      val annotations = property._4

      val colAnnotation = annotations.find(a => a.isInstanceOf[ColumnBase]).map(a => a.asInstanceOf[ColumnBase])
      
      /*
       * Retrieve the member in use, its class and its generic type
       */
      var (member, clsOfField, typeOfField) =
        (setter.map(s => (s: Member, s.getParameterTypes.head, s.getGenericParameterTypes.head))
          .orElse(getter.map(g => (g: Member, g.getReturnType, g.getGenericReturnType)))
          .orElse(field.map(f => (f: Member, f.getType, f.getType)))
          .getOrElse(org.squeryl.internals.Utils.throwError("invalid field group")))

      /*
       * Look for a value in the sample type.  If one exists and
       * it is not None, we can use it to deduce the Option type.   
       */
      var v: AnyRef =
         if(sampleInstance4OptionTypeDeduction != null) {
           field flatMap { f =>
             f.get(sampleInstance4OptionTypeDeduction) match {
               case a: AnyRef => Some(a)
               case _ => None
             }
           } orElse {
	             getter flatMap { _.invoke(sampleInstance4OptionTypeDeduction, _EMPTY_ARRAY : _*) match {
	               case a: AnyRef => Some(a)
	               case _ => None
	             }
             }
           } getOrElse
            createDefaultValue(fieldMapper, member, clsOfField, Some(typeOfField), colAnnotation)
         }
         else null

      if(v != null && v == None) // can't deduce the type from None keep trying
        v = null

      val constructorSuppliedDefaultValue = v

      var customTypeFactory: Option[AnyRef=>Product1[Any] with AnyRef] = None

      if(classOf[Product1[Any]].isAssignableFrom(clsOfField))
        customTypeFactory = _createCustomTypeFactory(fieldMapper, parentMetaData.clasz, clsOfField)

      if(customTypeFactory != None) {
        val f = customTypeFactory.get
        v = f(null) // this creates a dummy (sample) field
      }

      if(v == null)
        /*
         * If we have not yet been able to deduce the value of the field, delegate to createDefaultValue
         * in order to do so.
         */
        v = try {
          createDefaultValue(fieldMapper, member, clsOfField, Some(typeOfField), colAnnotation)
        }
        catch {
          case e:Exception => null
        }

      val deductionFailed =
        v match {
          case Some(None) => true
          case a:Any  => (v == null)
        }

      if(deductionFailed) {
        var errorMessage = "Could not deduce Option[] type of field '" + name + "' of class " + parentMetaData.clasz.getName
        if(!detectScalapOnClasspath()) errorMessage += "scalap option deduction not enabled. See: http://squeryl.org/scalap.html for more information."
        org.squeryl.internals.Utils.throwError(errorMessage)
      }

      val isOption = v.isInstanceOf[Some[_]]

      val typeOfFieldOrTypeOfOption =
        if(!isOption)
          v.getClass
        else
          v.asInstanceOf[Option[AnyRef]].get.getClass

      val primitiveFieldType =
        if(v.isInstanceOf[Product1[_]])
          v.asInstanceOf[Product1[Any]]._1.asInstanceOf[AnyRef].getClass
        else if(isOption && v.asInstanceOf[Option[AnyRef]].get.isInstanceOf[Product1[_]]) {
          //if we get here, customTypeFactory has not had a chance to get created
          customTypeFactory = _createCustomTypeFactory(fieldMapper, parentMetaData.clasz, typeOfFieldOrTypeOfOption)
          v.asInstanceOf[Option[AnyRef]].get.asInstanceOf[Product1[Any]]._1.asInstanceOf[AnyRef].getClass
        }
        else
          typeOfFieldOrTypeOfOption


      new FieldMetaData(
        parentMetaData,
        name,
        typeOfFieldOrTypeOfOption,
        primitiveFieldType,
        customTypeFactory,
        isOption,
        getter,
        setter,
        field,
        colAnnotation,
        isOptimisticCounter,
        constructorSuppliedDefaultValue)
    }
  }

  /**
   * creates a closure that takes a java.lang. primitive wrapper (ex.: java.lang.Integer) and
   * that creates an instance of a custom type with it, the factory accepts null to create
   * default values for non nullable primitive types (int, long, etc...)
   */
  private def _createCustomTypeFactory(fieldMapper: FieldMapper, ownerClass: Class[_], typeOfField: Class[_]): Option[AnyRef=>Product1[Any] with AnyRef] = {
    // run through the given class hierarchy and return the first method
    // which is called "value" and doesn't return java.lang.Object
    @tailrec
    def find(c: Class[_]): Option[Method] =
     if(c != null)
       c.getMethods.find(m => m.getName == "value" && m.getReturnType != classOf[java.lang.Object]) match {
         case Some(m) => Some(m)
         case None => find(c.getSuperclass)
       }
     else None

     // invoke the given constructor and expose possible exceptions to the caller.
    def invoke(c: Constructor[_], value: AnyRef) =
      try {
        c.newInstance(value).asInstanceOf[Product1[Any] with AnyRef]
      } catch {
        case ex: InvocationTargetException =>
          throw ex.getTargetException
      }

    find(typeOfField) flatMap(m => {
      val pType = m.getReturnType

      assert(fieldMapper.isSupported(pType),
        "enclosed type %s of CustomType %s is not a supported field type!"
        .format(pType.getName, typeOfField.getName))

      val c = typeOfField.getConstructor(pType)
      val defaultValue = createDefaultValue(fieldMapper, c, pType, None, None)

      if(defaultValue == null) None
      else
        Some((i: AnyRef) =>
          if(i == null) invoke(c, defaultValue)
          else invoke(c, i)
        )
    })
  }

  def defaultFieldLength(fieldType: Class[_], fmd: FieldMetaData) = {
    if(classOf[String].isAssignableFrom(fieldType))
      fmd.schema.defaultLengthOfString      
    else if(classOf[java.math.BigDecimal].isAssignableFrom(fieldType) || classOf[scala.math.BigDecimal].isAssignableFrom(fieldType)) {
      fmd.schema.defaultSizeOfBigDecimal._1
    }
    else {
      fmd.schema.fieldMapper.defaultColumnLength(fieldType)
    }
  }
  
  def detectScalapOnClasspath(): Boolean = {
    try {
      Class.forName("scala.tools.scalap.scalax.rules.scalasig.ByteCode")
      true
    }catch{
      case cnfe : ClassNotFoundException =>
        false

    }
  }

  def optionTypeFromScalaSig(member: Member): Option[Class[_]] = {
    val scalaSigOption = ScalaSigParser.parse(member.getDeclaringClass())
    scalaSigOption flatMap { scalaSig =>
      val syms = scalaSig.topLevelClasses
      // Print classes
      val baos = new ByteArrayOutputStream
      val stream = new PrintStream(baos)
      val printer = new ScalaSigPrinter(stream, true)
      for (c <- syms) {
        if(c.path == member.getDeclaringClass().getName())
        	printer.printSymbol(c)
      }
      val fullSig = baos.toString
      val matcher = """\s%s : scala.Option\[scala\.(\w+)\]?""".format(member.getName).r.pattern.matcher(fullSig)
      if (matcher.find) {
        matcher.group(1) match {
          case "Int" => Some(classOf[scala.Int])
          case "Short" => Some(classOf[scala.Short])
          case "Long" => Some(classOf[scala.Long])
          case "Double" => Some(classOf[scala.Double])
          case "Float" => Some(classOf[scala.Float])
          case "Boolean" => Some(classOf[scala.Boolean])
          case "Byte" => Some(classOf[scala.Byte])
          case "Char" => Some(classOf[scala.Char])
          case _ => None //Unknown scala primitive type?
        }
      } else
        None //Pattern was not found anywhere in the signature
    }
  }

  def createDefaultValue(fieldMapper: FieldMapper, member: Member, p: Class[_], t: Option[Type], optionFieldsInfo: Option[Column]): Object = {
    if (p.isAssignableFrom(classOf[Option[Any]])) {
      /*
       * First we'll look at the annotation if it exists as it's the lowest cost.
       */
       optionFieldsInfo.flatMap(ann => 
         if(ann.optionType != classOf[Object])
           Some(createDefaultValue(fieldMapper, member, ann.optionType, None, None))
          else None).orElse{
	      /*
	       * Next we'll try the Java generic type.  This will fail if the generic parameter is a primitive as
	       * we'll see Object instead of scala.X
	       */
	      t match {
	        case Some(pt: ParameterizedType) => {
	          pt.getActualTypeArguments.toList match {
	            case oType :: Nil => {
	              if(classOf[Class[_]].isInstance(oType)) {
	                /*
	                 * Primitive types are seen by Java reflection as classOf[Object], 
	                 * if that's what we find then we need to get the real value from @ScalaSignature
	                 */
	                val trueTypeOption = 
	                  if (classOf[Object] == oType && detectScalapOnClasspath()) optionTypeFromScalaSig(member) 
	                  else Some(oType.asInstanceOf[Class[_]])
	                trueTypeOption flatMap { trueType =>
	                  val deduced = createDefaultValue(fieldMapper, member, trueType, None, optionFieldsInfo)
	                  if (deduced != null)
	                    Some(deduced)
	                  else
	                    None //Couldn't create default for type param
	                }
	              } else{
	            	  None //Type parameter is not a Class
	              }
	            }
	            case _ => None //Not a single type parameter
	          }
	        }
	        case _ => None //Not a parameterized type
	      } 
      }
    } 
    else {      
      fieldMapper.trySampleValueFor(p)
    }
  }
}
