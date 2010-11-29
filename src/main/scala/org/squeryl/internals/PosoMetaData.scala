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
 

import java.lang.Class
import java.lang.annotation.Annotation
import net.sf.cglib.proxy.{Factory, Callback, Enhancer}
import java.lang.reflect.{Member, Constructor, Method, Field}
import collection.mutable.{HashSet, ArrayBuffer}
import org.squeryl.annotations._
import org.squeryl._
import dsl.CompositeKey

class PosoMetaData[T](val clasz: Class[T], val schema: Schema, val viewOrTable: View[T]) {

  override def toString =
    'PosoMetaData + "[" + clasz.getSimpleName + "]" + fieldsMetaData.mkString("(",",",")")

  def findFieldMetaDataForProperty(name: String) =
     fieldsMetaData.find(fmd => fmd.nameOfProperty == name)

  val isOptimistic = classOf[Optimistic].isAssignableFrom(clasz)
  
  val constructor =
    _const.headOption.orElse(error(clasz.getName +
            " must have a 0 param constructor or a constructor with only primitive types")).get

  /**
   * @arg fieldsMetaData the metadata of the persistent fields of this Poso
   * @arg primaryKey None if this Poso is not a KeyedEntity[], Either[a persistedField, a composite key]  
   */
  val (fieldsMetaData, primaryKey): (Iterable[FieldMetaData], Option[Either[FieldMetaData,Method]]) = {

    val isImplicitMode = _isImplicitMode

    val setters = new ArrayBuffer[Method]

    val sampleInstance4OptionTypeDeduction =
      try {
        constructor._1.newInstance(constructor._2 :_*).asInstanceOf[AnyRef];
      }
      catch {
        case e:IllegalArgumentException =>
          throw new RuntimeException("invalid constructor choice " + constructor._1, e)
        case e:Exception =>
          throw new RuntimeException("exception occurred while invoking constructor : " + constructor._1, e)
      }

    val members = new ArrayBuffer[(Member,HashSet[Annotation])]

    _fillWithMembers(clasz, members)

    val name2MembersMap =
      members.groupBy(m => {

        val n = m._1.getName
        val idx = n.indexOf("_$eq")
        if(idx != -1)
          n.substring(0, idx)
        else
          n
      })

    val fmds = new ArrayBuffer[FieldMetaData];

    for(e <- name2MembersMap) {
      val name = e._1
      val v = e._2

      var a:Set[Annotation] = Set.empty
      for(memberWithAnnotationTuple <- v)
        a = a.union(memberWithAnnotationTuple._2)

      val members = v.map(t => t._1)

      // here we do a filter and not a find, because there can be more than one setter/getter/field
      // with the same name, we want one that is not an erased type, excluding return and input type
      // of java.lang.Object does it.

      val o = classOf[java.lang.Object]

      val field =
        members.filter(m => m.isInstanceOf[Field]).
           map(m=> m.asInstanceOf[Field]).filter(f=> f.getType != o).headOption

      val getter =
        members.filter(m => m.isInstanceOf[Method] && m.getName == name).
          map(m=> m.asInstanceOf[Method]).filter(m=> m.getReturnType != o).headOption

      val setter =
        members.filter(m => m.isInstanceOf[Method] && m.getName.endsWith("_$eq")).
          map(m=> m.asInstanceOf[Method]).filter(m=> m.getParameterTypes.apply(0) != o).headOption

      val property = (field, getter, setter, a)

      if(isImplicitMode && _groupOfMembersIsProperty(property)) {
        fmds.append(FieldMetaData.factory.build(this, name, property, sampleInstance4OptionTypeDeduction, isOptimistic && name == "occVersionNumber"))
      }
    }
    
    var k = fmds.find(fmd => fmd.isIdFieldOfKeyedEntity)

    val compositePrimaryKeyGetter: Option[Method] =
      if(k != None) // can't have both PK Field and CompositePK
        None
      else {
        // verify if we have an 'id' method that is a composite key, in this case we need to construct a
        // FieldMetaData that will become the 'primaryKey' field of this PosoMetaData
        val isKE = classOf[KeyedEntity[Any]].isAssignableFrom(clasz)
        val isIKE = classOf[IndirectKeyedEntity[_,_]].isAssignableFrom(clasz)
        if(isKE || isIKE) {

          val pkMethod =
            if(isKE)
              clasz.getMethod("id")
            else
              clasz.getMethod("idField")

          assert(pkMethod != null, "method id or idField should exist in class " + clasz.getName)
          assert(classOf[CompositeKey].isAssignableFrom(pkMethod.getReturnType),
            " expected return type of CompositeKey on method " + pkMethod.getName + " in " + clasz.getName)

          Some(pkMethod)
        }
        else
          None
      }

    val metaDataForPk: Option[Either[FieldMetaData,Method]] =
      if(k != None)
        Some(Left(k.get))
      else if(compositePrimaryKeyGetter != None)
        Some(Right(compositePrimaryKeyGetter.get))
      else
        None
    
    (fmds, metaDataForPk) //: (Iterable[FieldMetaData], Option[Either[FieldMetaData,Method]])
  }

  def optimisticCounter =
    fieldsMetaData.find(fmd => fmd.isOptimisticCounter)

  if(isOptimistic)
    assert(optimisticCounter != None)

  def _const = {

    val r = new ArrayBuffer[(Constructor[_],Array[Object])]

//    for(ct <- clasz.getConstructors)
//      println("CT: " + ct.getParameterTypes.map(c=>c.getName).mkString(","))
    
    for(ct <- clasz.getConstructors)
      _tryToCreateParamArray(r, ct)

    r.sortWith(
      (a:(Constructor[_],Array[Object]),
       b:(Constructor[_],Array[Object])) => a._2.length < b._2.length
    )
  }

  def _tryToCreateParamArray(
    r: ArrayBuffer[(Constructor[_],Array[Object])],
    c: Constructor[_]): Unit = {

    val params: Array[Class[_]] = c.getParameterTypes

    if(params.length >= 1) {
      val cn = clasz.getName
      val test = params(0).getName + "$" + clasz.getSimpleName
      if(cn == test)
        error("inner classes are not supported, except when outer class is a singleton (object)\ninner class is : " + cn)
    }

    var res = new Array[Object](params.size)

    for(i <- 0 to params.length -1) {
      val v = FieldMetaData.createDefaultValue(clasz, params(i), None)
      res(i) = v
    }

    r.append((c, res))
  }

  private def _noOptionalColumnDeclared =
    error("class " + clasz.getName + " has an Option[] member with no Column annotation with optionType declared.")

  //def createSamplePoso[T](vxn: ViewExpressionNode[T], classOfT: Class[T]): T = {
    //Enhancer.create(classOfT, new PosoPropertyAccessInterceptor(vxn)).asInstanceOf[T]
  //}

  def createSample(cb: Callback) =
    FieldReferenceLinker.executeAndRestoreLastAccessedFieldReference(_builder(cb))

  private val _builder: (Callback) => T = {


    val e = new Enhancer
    e.setSuperclass(clasz)
    val pc: Array[Class[_]] = constructor._1.getParameterTypes
    val args:Array[Object] = constructor._2
    e.setUseFactory(true)

    (callB:Callback) => {

      val cb = new Array[Callback](1)
      cb(0) = callB
      e.setCallback(callB)
      //TODO : are we creating am unnecessary instance ?  
      val fac = e.create(pc , constructor._2).asInstanceOf[Factory]

      fac.newInstance(pc, constructor._2, cb).asInstanceOf[T]
    }
  }

  private def _isImplicitMode = {
    
    val rowAnnotation = clasz.getAnnotation(classOf[Row])

    rowAnnotation == null ||
     rowAnnotation.fieldToColumnCorrespondanceMode == FieldToColumnCorrespondanceMode.IMPLICIT
  }

  private def _groupOfMembersIsProperty(property: (Option[Field], Option[Method], Option[Method], Set[Annotation])): Boolean  = {
    
    if(property._4.find(an => an.isInstanceOf[Transient]) != None)
      return false    

    val hasAField = property._1 != None

    val hasGetter = property._2 != None &&
      ! classOf[java.lang.Void].isAssignableFrom(property._2.get.getReturnType) &&
      property._2.get.getParameterTypes.length == 0

    val hasSetter = property._3 != None &&
      property._3.get.getParameterTypes.length == 1
    
    val memberTypes = new ArrayBuffer[Class[_]]

    if(hasAField)
      memberTypes.append(property._1.get.getType)
    if(hasGetter)
      memberTypes.append(property._2.get.getReturnType)
    if(hasSetter)
      memberTypes.append(property._3.get.getParameterTypes.apply(0))    

    //not a property if it has no getter, setter or field
    if(memberTypes.size == 0)
      return false

    //verify that all types are compatible :
    val c = memberTypes.remove(0)
    for(c0 <- memberTypes) {
      if((!c0.isAssignableFrom(c)) && (!c.isAssignableFrom(c0)))
        return false
    }

    (hasAField, hasGetter, hasSetter) match {
      case (true,  false, false) => true
      case (false, true,  true)  => true
      case (true,  true,  true)  => true
      case (true,  true, false)  => true
      case a:Any => false
    }
  }

  private def _includeAnnotation(a: Annotation) =
   a.isInstanceOf[ColumnBase] || a.isInstanceOf[Transient] || a.isInstanceOf[OptionType]
  
  private def _addAnnotations(m: Field, s: HashSet[Annotation]) =
    for(a <- m.getAnnotations if _includeAnnotation(a))
      s.add(a)

  private def _addAnnotations(m: Method, s: HashSet[Annotation]) =
    for(a <- m.getAnnotations if _includeAnnotation(a))
      s.add(a)

  private def _includeFieldOrMethodType(c: Class[_]) =
    FieldMetaData.factory.isSupportedFieldType(c)
      //! classOf[Query[_]].isAssignableFrom(c)

  private def _fillWithMembers(clasz: Class[_], members: ArrayBuffer[(Member,HashSet[Annotation])]) {

    for(m <-clasz.getMethods if(m.getDeclaringClass != classOf[Object]) && _includeFieldOrMethodType(m.getReturnType)) {
      m.setAccessible(true)
      val t = (m, new HashSet[Annotation])
      _addAnnotations(m, t._2)
      members.append(t)
    }

    for(m <- clasz.getDeclaredFields if (m.getName.indexOf("$") == -1) && _includeFieldOrMethodType(m.getType)) {
      m.setAccessible(true)
      val t = (m, new HashSet[Annotation])
      _addAnnotations(m, t._2)
      members.append(t)
    }

    val c = clasz.getSuperclass

    if(c != null)
      _fillWithMembers(c, members)
  }
}


