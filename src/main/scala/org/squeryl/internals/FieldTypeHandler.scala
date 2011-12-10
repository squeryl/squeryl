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
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.sql.ResultSet
import org.squeryl.dsl._


object FieldMapperz extends FieldMapperz {
  import org.squeryl.PrimitiveTypeMode._
  
  register(byteTEF)
  register(intTEF)
  register(longTEF)
  register(floatTEF)
  register(doubleTEF)  
  register(bigDecimalTEF)
  
  register(binaryTEF)
  register(booleanTEF)
  register(stringTEF)
  register(timestampTEF)
  register(dateTEF)  
  register(uuidTEF)
  val re = enumValueTEF(DummyEnum.DummyEnumerationValue)    
  
 /**
   * Enumerations are treated differently, since the map method should normally
   * return the actual Enumeration#value, but given that an enum is not only
   * determined by the int value from the DB, but also the parent Enumeration
   * parentEnumeration.values.find(_.id == v), the conversion is done 
   * in FieldMetaData.canonicalEnumerationValueFor(i: Int) 
   */
  val z = new FieldAttributesBasedOnType[Any](
      new {
        def map(rs:ResultSet,i:Int) = rs.getInt(i)
      }, 
      re.defaultColumnLength, re.sample)
      
  m.put(z.clasz, z)
  m.put(z.clasz.getSuperclass, z)
  
}

class FieldMapperz {
  
  type MapperForReflection = {
    def map(rs:ResultSet,i:Int): Any
  }
   
  class FieldAttributesBasedOnType[A](val mapper: MapperForReflection, val defaultLength: Int, val sample: A) {

    val clasz: Class[_] = sample.getClass

    override def toString = 
      clasz.getCanonicalName + " --> " + mapper.getClass.getCanonicalName    
  }
  
  def isSupported(c: Class[_]) =
    lookup(c) != None ||
    c.isAssignableFrom(classOf[Some[_]]) ||
    classOf[Product1[Any]].isAssignableFrom(c)
  
  def defaultColumnLength(c: Class[_]) =
    get(c).defaultLength

  def resultSetHandlerFor(c: Class[_]): (ResultSet,Int) => AnyRef = {
    val fa = get(c) 
    (rs:ResultSet,i:Int) => {
       val z = fa.mapper.map(rs,i)
       if(rs.wasNull) null
       else z.asInstanceOf[AnyRef]
    }
  }
      
  
  private def get(c: Class[_]) =
    lookup(c).getOrElse(
      Utils.throwError("Usupported native type " + c.getCanonicalName + "," + c.getName + "\n" + m.mkString("\n")))
  
  def sampleValueFor(c: Class[_]): AnyRef =
    get(c).sample.asInstanceOf[AnyRef]
  
  private val m = new HashMap[Class[_],FieldAttributesBasedOnType[_]]
  
  def register[A](f: TypedExpressionFactory[A,_]) {
    val z = new FieldAttributesBasedOnType(f.thisMapper, f.defaultColumnLength, f.sample)
    m.put(z.clasz, z)
  }
    
  private def lookup(c: Class[_]) =
    if(!c.isPrimitive) 
      m.get(c)
    else c.getName match {
      case "int" => m.get(classOf[java.lang.Integer])
      case "long" => m.get(classOf[java.lang.Long])
      case "float" => m.get(classOf[java.lang.Float])
      case "byte" => m.get(classOf[java.lang.Byte])
      case "boolean" => m.get(classOf[java.lang.Boolean])
      case "double" => m.get(classOf[java.lang.Double])
      case "void" => None
    }           
}
