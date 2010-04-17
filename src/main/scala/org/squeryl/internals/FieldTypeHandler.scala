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
