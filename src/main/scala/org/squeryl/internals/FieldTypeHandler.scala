package org.squeryl.internals


trait FieldTypeHandler[T] {

  def handleType(t: Class[_]) =
    if(t.isAssignableFrom(classOf[Int]) || t.isAssignableFrom(classOf[java.lang.Integer]))
      handleIntType
    else if(t.isAssignableFrom(classOf[Long]) || t.isAssignableFrom(classOf[java.lang.Long]))
      handleLongType
    else if(t.isAssignableFrom(classOf[java.lang.String]))
      handleStringType
    else if(t.isAssignableFrom(classOf[Boolean]) || t.isAssignableFrom(classOf[java.lang.Boolean]))
      handleBooleanType
    else if(t.isAssignableFrom(classOf[Double]) || t.isAssignableFrom(classOf[java.lang.Double]))
      handleDoubleType
    else if(t.isAssignableFrom(classOf[Float]) || t.isAssignableFrom(classOf[java.lang.Float]))  
      handleFloatType
    else if(classOf[java.util.Date].isAssignableFrom(t))
      handleDateType
    else
      handleUnknownType(t)

  protected def handleIntType : T
  protected def handleStringType : T
  protected def handleBooleanType : T
  protected def handleDoubleType : T
  protected def handleDateType: T
  protected def handleLongType: T
  protected def handleFloatType: T

  protected def handleUnknownType(c: Class[_]) : T
}