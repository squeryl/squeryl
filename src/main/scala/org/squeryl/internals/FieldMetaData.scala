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
 ******************************************************************************/
package org.squeryl.internals

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method, ParameterizedType, Type, TypeVariable}
import java.sql.ResultSet
import java.math.BigDecimal
import org.squeryl.annotations.{ColumnBase, Column}


class FieldMetaData(
        val parentMetaData: PosoMetaData[_],
        val nameOfProperty:String,
        val fieldType: Class[_], // if isOption, this fieldType is the type param of Option, i.e. the T in Option[T]
        val wrappedFieldType: Class[_], //in primitive type mode fieldType == wrappedFieldType, in custom type mode wrappedFieldType is the 'real'
        // type, i.e. the (primitive) type that jdbc understands
        val customTypeFactory: Option[AnyRef=>Product1[Any]],
        val isOption: Boolean,
        getter: Option[Method],
        setter: Option[Method],
        field:  Option[Field],
        columnAnnotation: Option[Column],
        val isOptimisticCounter: Boolean) {


  private lazy val sampleValue = get(parentMetaData.sampleInstance)

  def isEnumeration = {
    classOf[Enumeration#Value].isAssignableFrom(wrappedFieldType)
  }

  def canonicalEnumerationValueFor(id: Int) =
    if(sampleValue == null) {
      error("classes with Enumerations must have a zero param constructor that assigns a sample to the enumeration field")
    }
    else {

      val svE =
        if(sampleValue.isInstanceOf[Option[_]])
          sampleValue.asInstanceOf[Option[Enumeration#Value]].get
        else 
          sampleValue.asInstanceOf[Enumeration#Value]

      val m = svE.getClass.getField("$outer")

      val enu = m.get(svE).asInstanceOf[Enumeration]

      val r = enu.values.find(_.id == id).get

      r
    }

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
      FieldMetaData.defaultFieldLength(wrappedFieldType, this)
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
    if(columnAnnotation == None)
      parentMetaData.schema.columnNameFromPropertyName(nameOfProperty)
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
    set(target, v)
  }
  
  /**
   * Sets the value 'v' to the object, the value will be converted to Some or None
   * if the field is an Option[], (if isOption).   
   */
  def set(target: AnyRef, v: AnyRef) = {
    try {
      val v0:AnyRef =
        if(isEnumeration) {
          if(v != null)
            canonicalEnumerationValueFor(v.asInstanceOf[java.lang.Integer].intValue)
          else
            null
        }
        else if(customTypeFactory == None)
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

trait FieldMetaDataFactory {

  def build(parentMetaData: PosoMetaData[_], name: String, property: (Option[Field], Option[Method], Option[Method], Set[Annotation]), isOptimisticCounter: Boolean): FieldMetaData

  def isSupportedFieldType(c: Class[_]): Boolean =
    FieldMetaData._isSupportedFieldType.handleType(c, None)      
}

object FieldMetaData {

  private val _EMPTY_ARRAY = new Array[Object](0)

  private val AnyClass = classOf[Any]
  private val OptionClass = classOf[Option[_]]
  private val Product1Class = classOf[Product1[_]]

  private [squeryl] val _isSupportedFieldType = new FieldTypeHandler[Boolean] {

    def handleIntType = true
    def handleStringType  = true
    def handleStringType(fmd: Option[FieldMetaData]) = true
    def handleBooleanType = true
    def handleDoubleType = true
    def handleDateType = true
    def handleLongType = true
    def handleFloatType = true
    def handleBigDecimalType(fmd: Option[FieldMetaData]) = true
    def handleTimestampType = true
    def handleBinaryType = true
    def handleEnumerationValueType = true
    def handleUnknownType(c: Class[_]) =
      c.isAssignableFrom(classOf[Some[_]]) ||
      classOf[Product1[Any]].isAssignableFrom(c)
        //classOf[Some[_]].isAssignableFrom(c)
  }

  final case class DeducedFieldType(isOption: Boolean, fieldValueType: Class[_], primitiveType: Class[_]) {
    def wrapWithCustomType(in: AnyRef): AnyRef =
      if (primitiveType != fieldValueType) {
        fieldValueType.getConstructors.find(_.getParameterTypes.length == 1).get.newInstance(in).asInstanceOf[AnyRef]
      } else {
        in
      }

    def createDefaultValue: AnyRef = {
      val v = wrapWithCustomType(_defaultValueFactory.handleType(primitiveType, None))
      if (isOption) Some(v)
      else v
    }
  }

  /**
   * Deduce the simplified type of a field, ensuring that a supported combination of Option / custom type nesting is being used.
   * Results in Left(errorText) in the case where the type is unsupported, or Right(DeducedFieldType) in the case where it is.
   */
  def deduceFieldTypeAndOption(in: Type): Either[String, DeducedFieldType] = {
    def cannotDeduceErasedType(what: String) =
      Left("cannot deduce type of " + what + " because the type was erased. this can occur when the field has no getter or setter")

    val invalidCustomInCustom = Left("nested custom types are not supported -- that is, CustomType[CustomType[T]] not supported")
    val invalidOptionInOption = Left("Option[Option[T]] not supported")
    val invalidOptionInCustom = Left("primitive type of a custom type cannot be option -- that is, CustomType[Option[T]] not supported")

    val invalidUseOfProduct1 = Left("Product1[T] cannot be used directly -- custom types are types that conform to Product1[T] and have " + 
                                    "a single argument constructor that can be used to wrap a primitive type")

    def withOneTypeArgument[A](pt: ParameterizedType, f: Type => Either[String, A]): Either[String, A] =
      pt.getActualTypeArguments.toList match {
        case ty :: Nil => f(ty)
        case _ => Left("wrong number of type arguments for " + pt.getRawType)
      }

    def unsupportedType(t: Type) =
      t match {
        case AnyClass =>
          Left("unsupported field value type Any -- usually this occurs when type erasure has lost the type. Ensure that your field " +
               "values are of some refined type, e.g. Option[String] not Option[T]")

        case _ => 
          Left("unsupported field value type " + t + " -- only primitive types, " +
               "possibly wrapped by Options or custom types (Product1) are supported")
      }

    /*
     * Helper function that does the complicated deduction of what type T some subtype of Product1[T] has. This is
     * complicated because of two factors -- first is that we have to walk the supertypes (superclass and interfaces) of the
     * leaf type recursively to go find the Product1[T], and second is that it might literally be Product1[T] so we have to
     * resolve type variables along the way while walking supertypes.
     *
     * If this sounds unlikely, it isn't. In fact it occurs in Squeryl itself --
     *   IntField <: CustomType[Int] <: Product1[T]
     */
    def deduceTypeOfProduct1Subtype(in: Type): Either[String, DeducedFieldType] = {
      /* Helper function that replaces any TypeVariables inside ParameterizedType "in" with the type argument from context */
      def resolvePT(context: ParameterizedType)(in: Type): Type =
        in match {
          case (pt: ParameterizedType) if (context.getRawType.isInstanceOf[Class[_]] &&
                                           context.getRawType.asInstanceOf[Class[_]].getTypeParameters.length > 0) => {
            val contextArgs = context.getActualTypeArguments
            val paramSlots = Map(context.getRawType.asInstanceOf[Class[_]].getTypeParameters.map(_.getName).zipWithIndex: _*)
            val resolvedArgs = pt.getActualTypeArguments.map {
              case (tv: TypeVariable[_]) if paramSlots contains tv.getName => contextArgs(paramSlots(tv.getName))
              case other => other
            }
            new ParameterizedType {
              def getActualTypeArguments = resolvedArgs
              def getOwnerType = pt.getOwnerType
              def getRawType = pt.getRawType
              override def toString = getRawType.toString + "<" + getActualTypeArguments.mkString(", ") + ">"
            }
          }

          case other => other
        }

      /** Recursive function that collects all type arguments to Product1 anywhere in the supertypes of "in" */
      def collectProduct1Types(in: Type, rest: List[Class[_]]): List[Class[_]] =
        in match {
          case Product1Class => rest // can't figure out what the type of it was
          case (c: Class[_]) => {
            // no type arguments to pass down, so let any TypeVariables go down unhindered
            val toTry = if (c.getGenericSuperclass eq null) c.getGenericInterfaces.toList
                        else c.getGenericSuperclass :: c.getGenericInterfaces.toList
            toTry.foldLeft(rest)((prev, cur) => collectProduct1Types(cur, prev))
          }

          case (pt: ParameterizedType) =>
            pt.getRawType match {
              case Product1Class =>
                pt.getActualTypeArguments.toList match {
                  case (c: Class[_]) :: Nil => c :: rest
                  case _ => Nil
                }
                
              case (c: Class[_]) => {
                val toTry = if (c.getGenericSuperclass eq null) c.getGenericInterfaces.toList
                            else c.getGenericSuperclass :: c.getGenericInterfaces.toList
                toTry.map(resolvePT(pt)).foldLeft(rest)((prev, cur) => collectProduct1Types(cur, prev))
              }

              case _ => rest
            }

          case _ => Nil
        }

      // presume that there are not incompatible instances of Product1, e.g. Product1[String] and Product1[Double]
      val innerTypePart: Either[String, Class[_]] =
        collectProduct1Types(in, Nil).sortWith(_ isAssignableFrom _).reverse.headOption match {
          case None =>
            Left("could not find type of Product1 by searching supertypes of " + in + " -- looked everywhere for Product1[simple class]")

          case Some(AnyClass) => 
            Left("could not find type of Product1 by searching supertypes of " + in +
                 " -- found Product1[Any], which usually means the actual type was lost in type erasure")

          case Some(c) if _defaultValueFactory.handleType(c, None) ne null =>
            Right(c)

          case Some(c) =>
            Left("found Product1[" + c.getName + "] -- but that type is not a primitive type, so is unsupported")
        }

      val outerTypePart: Either[String, Class[_]] =
        in match {
          case (c: Class[_]) => Right(c)
          case (pt: ParameterizedType) =>
            pt.getRawType match {
              case (c: Class[_]) => Right(c)
              case other => Left("custom type is " + pt + " has a non-Class raw type which is unsupported")
            }

          case other => Left("custom type " + in + " is neither a Class nor a ParameterizedType and is unsupported")
        }

      (outerTypePart, innerTypePart) match {
        case (Left(err), _) => Left(err)
        case (_, Left(err)) => Left(err)
        case (Right(outer), Right(inner)) => Right(DeducedFieldType(false, outer, inner))
      }
    }

    /*
     * Helper function that does the main type deduction, returning Left for various error conditions and Right for a deduced
     * type.
     * 
     * insideOption and insideCustomType are passed in during recursion to disallow certain types of nesting -- Option[T] is
     * now allowed when insideOption or insideCustomType, and custom types are not supported when already inside a custom type.
     */
    def deduce(in: Type, insideOption: Boolean, insideCustomType: Boolean): Either[String, DeducedFieldType] =
      in match {
        // Handle the simple case of a primitive type.
        case (c: Class[_]) if _defaultValueFactory.handleType(c, None) ne null => Right(DeducedFieldType(false, c, c))

        // Error out if we see an erased Option or Product1, since we don't have enough information to deduce
        case OptionClass   => cannotDeduceErasedType("Option")

        // Product1[T] by itself cannot be supported (could not be instantiated) so error out in that case
        case Product1Class => invalidUseOfProduct1
        case (pt: ParameterizedType) if pt.getRawType == Product1Class => invalidUseOfProduct1

        // Error out on incorrect nesting
        case (c: Class[_]) if Product1Class.isAssignableFrom(c) && insideCustomType => invalidCustomInCustom

        // Do the complicated deduction of a Product1 subclass
        case (c: Class[_]) if Product1Class.isAssignableFrom(c) => deduceTypeOfProduct1Subtype(c)

        case (pt: ParameterizedType) =>
          pt.getRawType match {
            // Error out on incorrect nesting
            case OptionClass   if insideOption     => invalidOptionInOption
            case OptionClass   if insideCustomType => invalidOptionInCustom
            case Product1Class if insideCustomType => invalidCustomInCustom
            case (c: Class[_]) if Product1Class.isAssignableFrom(c) && insideCustomType => invalidCustomInCustom

            // Properly parameterized Option[T]
            case OptionClass => withOneTypeArgument(pt, ty => deduce(ty, true, false).right.map(_.copy(isOption = true)))

            // Likely but complicated case of some subtype of Product[1] that happens to be parameterized
            case (c: Class[_]) if Product1Class.isAssignableFrom(c) => deduceTypeOfProduct1Subtype(pt)

            case other => unsupportedType(other)
          }

        case other => unsupportedType(other)
      }

    deduce(in, false, false)
  }

  var factory = new FieldMetaDataFactory {   
    
    def build(parentMetaData: PosoMetaData[_], name: String, property: (Option[Field], Option[Method], Option[Method], Set[Annotation]), isOptimisticCounter: Boolean) = {

      val field  = property._1
      val getter = property._2
      val setter = property._3
      val annotations = property._4

      val colAnnotation = annotations.find(a => a.isInstanceOf[ColumnBase]).map(a => a.asInstanceOf[ColumnBase])

      val typeOfField =
        (setter.flatMap(_.getGenericParameterTypes.headOption)
         .orElse(getter.map(_.getGenericReturnType))
         .orElse(field.map(_.getType))
         .getOrElse(error("invalid field group")))

      val deducedFieldType@DeducedFieldType(isOption, fieldValueType, primitiveFieldType) =
        deduceFieldTypeAndOption(typeOfField).fold(
          s => error("cannot deduce field type for " + name + " in " + parentMetaData.clasz.getName + ": " + s),
          identity
        )

      var customTypeFactory: Option[AnyRef=>Product1[Any]] =
        if (fieldValueType != primitiveFieldType) _createCustomTypeFactory(parentMetaData.clasz, deducedFieldType)
        else None

      new FieldMetaData(
        parentMetaData,
        name,
        fieldValueType,
        primitiveFieldType,
        customTypeFactory,
        isOption,
        getter,
        setter,
        field,
        colAnnotation,
        isOptimisticCounter)
    }
  }

  /**
   * creates a closure that takes a java.lang. primitive wrapper (ex.: java.lang.Integer) and
   * that creates an instance of a custom type with it, the factory accepts null to create
   * default values for non nullable primitive types (int, long, etc...)
   */
  private def _createCustomTypeFactory(ownerClass: Class[_], deducedType: DeducedFieldType): Option[AnyRef=>Product1[Any]] = {
    for(c <- deducedType.fieldValueType.getConstructors if c.getParameterTypes.length == 1) {
      return  Some(
        (i:AnyRef)=> {
          if(i != null)
            c.newInstance(i).asInstanceOf[Product1[Any]]
          else
            deducedType.createDefaultValue.asInstanceOf[Product1[Any]]
        }
      )
    }

    None
  }

  def defaultFieldLength(fieldType: Class[_], fmd: FieldMetaData) =
    _defaultFieldLengthAssigner.handleType(fieldType, Some(fmd))

  private val _defaultFieldLengthAssigner = new FieldTypeHandler[Int] {

    def handleIntType = 4
    def handleStringType  = 255
    def handleStringType(fmd: Option[FieldMetaData]) = fmd.get.schema.defaultLengthOfString
    def handleBooleanType = 1
    def handleDoubleType = 8
    def handleDateType = -1
    def handleLongType = 8
    def handleFloatType = 4
    def handleBigDecimalType(fmd: Option[FieldMetaData]) = fmd.get.schema.defaultSizeOfBigDecimal._1
    def handleTimestampType = -1
    def handleBinaryType = 255
    def handleEnumerationValueType = 4
    def handleUnknownType(c: Class[_]) = error("Cannot assign field length for " + c.getName)
  }

  private val _defaultValueFactory = new FieldTypeHandler[AnyRef] {

    def handleIntType = new java.lang.Integer(0)
    def handleStringType  = ""
    def handleStringType(fmd: Option[FieldMetaData])  = ""
    def handleBooleanType = new java.lang.Boolean(false)
    def handleDoubleType = new java.lang.Double(0.0)
    def handleDateType = new java.util.Date()
    def handleLongType = new java.lang.Long(0)
    def handleFloatType = new java.lang.Float(0)
    def handleBigDecimalType(fmd: Option[FieldMetaData]) = new scala.math.BigDecimal(java.math.BigDecimal.ZERO)
    def handleTimestampType = new java.sql.Timestamp(0)
    def handleBinaryType = new Array[Byte](0)
    def handleEnumerationValueType = DummyE.Z
    def handleUnknownType(c: Class[_]) = null
  }

  object DummyE extends Enumeration {
    type DummyE = Value
    val Z = Value
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
    val _bigDecM =  (rs:ResultSet,i:Int) => _handleNull(rs, new scala.math.BigDecimal(rs.getBigDecimal(i)))
    val _timestampM =    (rs:ResultSet,i:Int) => _handleNull(rs, rs.getTimestamp(i))
    val _binaryM =  (rs:ResultSet,i:Int) => _handleNull(rs, rs.getBytes(i))

    def handleIntType = _intM
    def handleStringType  = _stringM
    def handleStringType(fmd: Option[FieldMetaData])  = _stringM
    def handleBooleanType = _booleanM
    def handleDoubleType = _doubleM
    def handleDateType = _dateM
    def handleFloatType = _floatM
    def handleLongType = _longM
    def handleBigDecimalType = _bigDecM
    def handleBigDecimalType(fmd: Option[FieldMetaData]) = _bigDecM
    def handleTimestampType = _timestampM
    def handleBinaryType = _binaryM
    def handleEnumerationValueType = _intM

    def handleUnknownType(c: Class[_]) =
      error("field type " + c.getName + " is not supported")
  }

  def resultSetHandlerFor(c: Class[_]) =
    _mapper.handleType(c, None)

  def createDefaultValue(ownerClass: Class[_], p: Type): Object =
    deduceFieldTypeAndOption(p).fold(
        s => error("cannot deduce field type in " + ownerClass.getName + ": " + s),
        dft => dft.createDefaultValue
    )
}
