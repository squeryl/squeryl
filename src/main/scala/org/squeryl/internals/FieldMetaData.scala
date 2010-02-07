package org.squeryl.internals

import org.squeryl.annotations.Column
import java.lang.annotation.Annotation
import org.squeryl.CustomType
import java.lang.reflect.{Constructor, Field, Member, Method}
import java.sql.ResultSet

class FieldMetaData(
        val parentMetaData: PosoMetaData[_],
        val nameOfProperty:String,
        val fieldType: Class[_], // if isOption, this fieldType is the type param of Option, i.e. the T in Option[T]
        val wrappedFieldType: Class[_], //in primitive type mode fieldType == wrappedFieldType, in custom type mode wrappedFieldType is the 'real'
        // type, i.e. the (primitive) type that jdbc understands
        val customTypeFactory: Option[AnyRef=>CustomType],
        val isOption: Boolean,
        getter: Option[Method],
        setter: Option[Method],
        field:  Option[Field],
        columnAnnotation: Option[Column]) {

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
    if(columnAnnotation == None || columnAnnotation.get.length == -1)
      FieldMetaData.defaultFieldLength(wrappedFieldType)
    else
      columnAnnotation.get.length

  /**
   * The name of the database column
   */
  def columnName =
    if(columnAnnotation == None || columnAnnotation.get.name == "")
      nameOfProperty
    else
      columnAnnotation.get.name
  
  val resultSetHandler =
    FieldMetaData.resultSetHandlerFor(wrappedFieldType)

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

  lazy val isPrimaryKey =
    parentMetaData.primaryKey != None &&
    this == parentMetaData.primaryKey.get

  var isAutoIncremented = false

  /**
   * gets the value of the field from the object.
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
      case e: IllegalArgumentException => error(wrappedFieldType.getName + " used on " + o.getClass.getName)
    }

  def setFromResultSet(target: AnyRef, rs: ResultSet, index: Int) = {
    val v = resultSetHandler(rs, index)    
    _set(target, v)
  }
  
  /**
   * Sets the value 'v' to the object, the value will be converted to Some or None
   * if the field is an Option[], (if isOption).   
   */
  private def _set(target: AnyRef, v: AnyRef) = {
    try {
      val v0:AnyRef =
        if(customTypeFactory == None)
          v
        else {
          val f = customTypeFactory.get
          f(v)
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
      case e: IllegalArgumentException => {
        val typeOfV = if(v == null) "null" else v.getClass.getName
        error(
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

object FieldMetaData {

  private val _EMPTY_ARRAY = new Array[Object](0)

  def build(parentMetaData: PosoMetaData[_], name: String, property: (Option[Field], Option[Method], Option[Method], Set[Annotation]), sampleInstance4OptionTypeDeduction: AnyRef) = {

    val field  = property._1
    val getter = property._2
    val setter = property._3
    val annotations = property._4

    val colAnnotation = annotations.find(a => a.isInstanceOf[Column]).map(a => a.asInstanceOf[Column]) 

    var typeOfField =
      if(setter != None)
        setter.get.getParameterTypes.apply(0)
      else if(getter != None)
        getter.get.getReturnType
      else if(field != None)
        field.get.getType
      else
        error("invalid field group")

    var v =
       if(sampleInstance4OptionTypeDeduction != null) {
         if(field != None)
           field.get.get(sampleInstance4OptionTypeDeduction)
         else if(getter != None)
           getter.get.invoke(sampleInstance4OptionTypeDeduction, _EMPTY_ARRAY :_*)
         else
           createDefaultValue(parentMetaData.clasz, typeOfField, colAnnotation)
       }
       else null

    if(v != null && v == None) // can't deduce the type from None in this case the Annotation
      v = null         //needs to tell us the type, if it doesn't it will a few lines bellow

    var customTypeFactory: Option[AnyRef=>CustomType] = None

    if(classOf[CustomType].isAssignableFrom(typeOfField))
      customTypeFactory = _createCustomTypeFactory(parentMetaData.clasz, typeOfField)

    if(customTypeFactory != None) {
      val f = customTypeFactory.get
      v = f(null) // this creates a dummy (sample) field
    }

    if(v == null)
      v = try {
        createDefaultValue(parentMetaData.clasz, typeOfField, colAnnotation)
      }
      catch {
        case e:Exception => null 
      }

    if(v == null)
      error("Could not deduce Option[] type of field '" + name + "' of class " + parentMetaData.clasz.getName)
    
    val isOption = v.isInstanceOf[Some[_]]

    val typeOfFieldOrTypeOfOption =
      if(!isOption)
        v.getClass
      else
        v.asInstanceOf[Option[AnyRef]].get.getClass

    val primitiveFieldType =
      if(v.isInstanceOf[CustomType])
        v.asInstanceOf[CustomType].wrappedValue.asInstanceOf[AnyRef].getClass
      else if(isOption && v.asInstanceOf[Option[AnyRef]].get.isInstanceOf[CustomType]) {
        //if we get here, customTypeFactory has not had a chance to get created 
        customTypeFactory = _createCustomTypeFactory(parentMetaData.clasz, typeOfFieldOrTypeOfOption)
        v.asInstanceOf[Option[AnyRef]].get.asInstanceOf[CustomType].wrappedValue.asInstanceOf[AnyRef].getClass
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
      colAnnotation)
  }

  /**
   * creates a closure that takes a java.lang. primitive wrapper (ex.: java.lang.Integer) and
   * that creates an instance of a custom type with it, the factory accepts null to create
   * default values for non nullable primitive types (int, long, etc...)
   */
  private def _createCustomTypeFactory(ownerClass: Class[_], typeOfField: Class[_]): Option[AnyRef=>CustomType] = {
    for(c <- typeOfField.getConstructors if c.getParameterTypes.length == 1) {
      val pTypes = c.getParameterTypes
      val dv = createDefaultValue(ownerClass, pTypes(0), None)
      if(dv != null)
        return  Some(
          (i:AnyRef)=> {
            if(i != null)
              c.newInstance(i).asInstanceOf[CustomType]
            else
              c.newInstance(dv).asInstanceOf[CustomType]
          }
        )
    }

    None
  }

  def defaultFieldLength(fieldType: Class[_]) =
    _defaultFieldLengthAssigner.handleType(fieldType) 

  private val _defaultFieldLengthAssigner = new FieldTypeHandler[Int] {

    def handleIntType = 4
    def handleStringType  = 128
    def handleBooleanType = 1
    def handleDoubleType = 8
    def handleDateType = -1
    def handleLongType = 8
    def handleFloatType = 4
    def handleUnknownType(c: Class[_]) = error("Cannot assign field length for " + c.getName)
  }

  private val _defaultValueFactory = new FieldTypeHandler[AnyRef] {

    def handleIntType = new java.lang.Integer(0)
    def handleStringType  = ""
    def handleBooleanType = new java.lang.Boolean(false)
    def handleDoubleType = new java.lang.Double(0.0)
    def handleDateType = new java.util.Date()
    def handleLongType = new java.lang.Long(0)
    def handleFloatType = new java.lang.Float(0)
    def handleUnknownType(c: Class[_]) = null
  }

  private val _mapper = new FieldTypeHandler[(ResultSet,Int)=>AnyRef] {

    private def _handleNull(rs: ResultSet, v: Any) =
      if(rs.wasNull)
        null
      else
        v.asInstanceOf[AnyRef]

    val _intM =     (rs:ResultSet,i:Int) => _handleNull(rs, rs.getInt(i))
    val _stringM =  (rs:ResultSet,i:Int) => _handleNull(rs, rs.getString(i))
    val _doubleM =  (rs:ResultSet,i:Int) => _handleNull(rs, rs.getDouble(i))
    val _booleanM = (rs:ResultSet,i:Int) => _handleNull(rs, rs.getBoolean(i))
    //(rs:ResultSet,i:Int) => Session.currentSession.databaseAdapter.convertToBooleanForJdbc(rs, i)
    val _dateM =    (rs:ResultSet,i:Int) => _handleNull(rs, rs.getDate(i))
    val _longM =    (rs:ResultSet,i:Int) => _handleNull(rs, rs.getLong(i))
    val _floatM =   (rs:ResultSet,i:Int) => _handleNull(rs, rs.getFloat(i))

    def handleIntType = _intM
    def handleStringType  = _stringM
    def handleBooleanType = _booleanM
    def handleDoubleType = _doubleM
    def handleDateType = _dateM
    def handleFloatType = _floatM
    def handleLongType = _longM
    
    def handleUnknownType(c: Class[_]) =
      error("field type " + c.getName + " is not supported")
  }

  def resultSetHandlerFor(c: Class[_]) =
    _mapper.handleType(c)  

//  def createDefaultValue(ownerCLass: Class[_], p: Class[_], optionFieldsInfo: Array[Annotation]): Object =
//    createDefaultValue(ownerCLass, p, optionFieldsInfo.find(a => a.isInstanceOf[Column]).map(a => a.asInstanceOf[Column]))

  def createDefaultValue(ownerCLass: Class[_], p: Class[_], optionFieldsInfo: Option[Column]): Object = {

    if(p.isAssignableFrom(classOf[Option[Any]])) {

//      if(optionFieldsInfo == None)
//        error("Option[Option[]] fields in "+ownerCLass.getName+ " are not supported")
//
//      if(optionFieldsInfo.size == 0)
//        return null
//      val oc0 = optionFieldsInfo.find(a => a.isInstanceOf[Column])
//      if(oc0 == None)
//        return null
//
//      val oc = oc0.get.asInstanceOf[Column]

      if(optionFieldsInfo == None)
        return null

      if(classOf[Object].isAssignableFrom(optionFieldsInfo.get.optionType))
        error("cannot deduce type of Option[] in " + ownerCLass.getName)

      Some(createDefaultValue(ownerCLass, optionFieldsInfo.get.optionType, optionFieldsInfo))
    }
    else
      _defaultValueFactory.handleType(p)
  }
}