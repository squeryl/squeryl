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
import net.sf.cglib.proxy.{Callback, CallbackFilter, Enhancer, Factory, NoOp}

import java.lang.reflect.{Constructor, Field, Member, Method, Modifier}
import collection.mutable.{ArrayBuffer, HashSet}
import org.squeryl.annotations._
import org.squeryl._

import scala.annotation.tailrec

class PosoMetaData[T](val clasz: Class[T], val schema: Schema, val viewOrTable: View[T]) {

  override def toString =
    "'PosoMetaData[" + clasz.getSimpleName + "]" + fieldsMetaData.mkString("(", ",", ")")

  def findFieldMetaDataForProperty(name: String) =
    _fieldsMetaData.find(fmd => fmd.nameOfProperty == name)

  val isOptimistic = viewOrTable.ked.exists(_.isOptimistic)

  val constructor =
    _const.headOption
      .orElse(
        org.squeryl.internals.Utils.throwError(
          clasz.getName +
            " must have a 0 param constructor or a constructor with only primitive types"
        )
      )
      .get

  def fieldsMetaData =
    _fieldsMetaData.filter(!_.isTransient)

  /**
   * fieldsMetaData the metadata of the persistent fields of this Poso
   * primaryKey None if this Poso is not a KeyedEntity[], Either[a persistedField, a composite key]
   */
  val (_fieldsMetaData, primaryKey): (Iterable[FieldMetaData], Option[Either[FieldMetaData, Method]]) = {

    val isImplicitMode = _isImplicitMode

    val sampleInstance4OptionTypeDeduction =
      try {
        constructor._1.newInstance(constructor._2: _*).asInstanceOf[AnyRef];
      } catch {
        case e: IllegalArgumentException =>
          throw new RuntimeException("invalid constructor choice " + constructor._1, e)
        case e: Exception =>
          throw new RuntimeException("exception occurred while invoking constructor : " + constructor._1, e)
      }

    val members = new ArrayBuffer[(Member, HashSet[Annotation])]

    _fillWithMembers(clasz, members)

    val name2MembersMap =
      members.groupBy(m => {

        val n = m._1.getName
        val idx = n.indexOf("_$eq")
        if (idx != -1)
          n.substring(0, idx)
        else
          n
      })

    val fmds = new ArrayBuffer[FieldMetaData];

    for (e <- name2MembersMap) {
      val name = e._1
      val v = e._2

      var a: Set[Annotation] = Set.empty
      for (memberWithAnnotationTuple <- v)
        a = a.union(memberWithAnnotationTuple._2)

      val members = v.map(t => t._1)

      // here we do a filter and not a find, because there can be more than one setter/getter/field
      // with the same name, we want one that is not an erased type, excluding return and input type
      // of java.lang.Object does it.

      val o = classOf[java.lang.Object]

      val field =
        members.collectFirst { case f: Field if f.getType != o => f }

      val getter =
        members.collectFirst { case m: Method if (m.getName == name) && (m.getReturnType != o) => m }

      val setter =
        members.collectFirst {
          case m: Method if m.getName.endsWith("_$eq") && (m.getParameterTypes.apply(0) != o) => m
        }

      val property = (field, getter, setter, a)

      if (isImplicitMode && _groupOfMembersIsProperty(property)) {
        val isOptimisitcCounter =
          (for (
            k <- viewOrTable.ked;
            counterProp <- k.optimisticCounterPropertyName if counterProp == name
          ) yield true).isDefined
        try {
          val optionFieldInnerClass = viewOrTable.optionalFieldsInfo.flatMap(_.get(name))
          val r = FieldMetaData.factory.build(
            this,
            name,
            property,
            sampleInstance4OptionTypeDeduction,
            isOptimisitcCounter,
            optionFieldInnerClass
          )
          fmds.append(r)
        } catch {
          case e: Exception =>
            println(">>>" + clasz.getCanonicalName)
            println(">>>" + name)

            throw new RuntimeException(
              Utils.failSafeString(
                "error while reflecting on metadata for " + property +
                  " of class " + this.clasz.getCanonicalName
              ),
              e
            )
        }
      }
    }

    val k = fmds.find(fmd => fmd.isIdFieldOfKeyedEntity)

    val compositePrimaryKeyGetter: Option[Method] =
      if (k.isDefined) // can't have both PK Field and CompositePK
        None
      else {
        // verify if we have an 'id' method that is a composite key, in this case we need to construct a
        // FieldMetaData that will become the 'primaryKey' field of this PosoMetaData

        viewOrTable.ked.map { ked =>

          val pkMethod = clasz.getMethod(ked.idPropertyName)

          assert(pkMethod != null, "Could not get getter for " + ked.idPropertyName + " in " + clasz.getCanonicalName())

          pkMethod
        }
      }

    val metaDataForPk: Option[Either[FieldMetaData, Method]] =
      if (k.isDefined)
        Some(Left(k.get))
      else if (compositePrimaryKeyGetter.isDefined)
        Some(Right(compositePrimaryKeyGetter.get))
      else
        None

    (fmds, metaDataForPk) // : (Iterable[FieldMetaData], Option[Either[FieldMetaData,Method]])
  }

  def optimisticCounter =
    fieldsMetaData.find(fmd => fmd.isOptimisticCounter)

  if (isOptimistic)
    assert(optimisticCounter.isDefined)

  def _const = {

    val r = new ArrayBuffer[(Constructor[_], Array[Object])]

//    for(ct <- clasz.getConstructors)
//      println("CT: " + ct.getParameterTypes.map(c=>c.getName).mkString(","))

    for (ct <- clasz.getConstructors)
      _tryToCreateParamArray(r, ct)

    r.sortWith((a: (Constructor[_], Array[Object]), b: (Constructor[_], Array[Object])) => a._2.length < b._2.length)
  }

  def _tryToCreateParamArray(r: ArrayBuffer[(Constructor[_], Array[Object])], c: Constructor[_]): Unit = {

    val params: Array[Class[_]] = c.getParameterTypes

    if (params.length >= 1) {
      val cn = clasz.getName
      val test = params(0).getName + "$" + clasz.getSimpleName
      if (cn == test)
        org.squeryl.internals.Utils.throwError(
          "inner classes are not supported, except when outer class is a singleton (object)\ninner class is : " + cn
        )
    }

    val res = new Array[Object](params.length)

    for (i <- params.indices) {
      val v = FieldMetaData.createDefaultValue(schema.fieldMapper, c, params(i), None, None, None)
      res(i) = v
    }

    r.append((c, res))
  }

  // def createSamplePoso[T](vxn: ViewExpressionNode[T], classOfT: Class[T]): T = {
  // Enhancer.create(classOfT, new PosoPropertyAccessInterceptor(vxn)).asInstanceOf[T]
  // }

  def createSample(cb: Callback) =
    FieldReferenceLinker.executeAndRestoreLastAccessedFieldReference(_builder(cb))

  private[this] val _builder: (Callback) => T = {

    val e = new Enhancer
    e.setSuperclass(clasz)
    val pc: Array[Class[_]] = constructor._1.getParameterTypes
    e.setUseFactory(true)

    (callB: Callback) => {

      val cb = Array[Callback](callB, NoOp.INSTANCE)
      e.setCallbacks(cb)
      e.setCallbackFilter(PosoMetaData.finalizeFilter)
      // TODO : are we creating am unnecessary instance ?
      val fac = e.create(pc, constructor._2).asInstanceOf[Factory]

      fac.newInstance(pc, constructor._2, cb).asInstanceOf[T]
    }
  }

  private def _isImplicitMode = {

    val rowAnnotation = clasz.getAnnotation(classOf[Row])

    rowAnnotation == null ||
    rowAnnotation.fieldToColumnCorrespondanceMode == FieldToColumnCorrespondanceMode.IMPLICIT
  }

  private def _groupOfMembersIsProperty(
    property: (Option[Field], Option[Method], Option[Method], Set[Annotation])
  ): Boolean = {

    if (property._4.find(an => an.isInstanceOf[Transient]).isDefined)
      return false

    val hasAField = property._1.exists { field =>
      !Modifier.isStatic(field.getModifiers)
    }

    val hasGetter = property._2.exists { getter =>
      !Modifier.isStatic(getter.getModifiers) &&
      !classOf[java.lang.Void].isAssignableFrom(getter.getReturnType) &&
      getter.getParameterTypes.length == 0
    }

    val hasSetter = property._3.exists { setter =>
      !Modifier.isStatic(setter.getModifiers) &&
      property._3.get.getParameterTypes.length == 1
    }

    val memberTypes = new ArrayBuffer[Class[_]]

    if (hasAField)
      memberTypes.append(property._1.get.getType)
    if (hasGetter)
      memberTypes.append(property._2.get.getReturnType)
    if (hasSetter)
      memberTypes.append(property._3.get.getParameterTypes.apply(0))

    // not a property if it has no getter, setter or field
    if (memberTypes.isEmpty)
      return false

    // verify that all types are compatible :
    val c = memberTypes.remove(0)
    if (
      memberTypes.exists { c0 =>
        (!c0.isAssignableFrom(c)) && (!c.isAssignableFrom(c0))
      }
    ) {
      false
    } else {
      (hasAField, hasGetter, hasSetter) match {
        case (true, false, false) => true
        case (false, true, true) => true
        case (true, true, true) => true
        case (true, true, false) => true
        case _ => false
      }
    }
  }

  private def _includeAnnotation(a: Annotation) =
    a.isInstanceOf[ColumnBase] || a.isInstanceOf[Transient] || a.isInstanceOf[OptionType]

  private def _addAnnotations(m: Field, s: HashSet[Annotation]) =
    for (a <- m.getAnnotations if _includeAnnotation(a))
      s.add(a)

  private def _addAnnotations(m: Method, s: HashSet[Annotation]) =
    for (a <- m.getAnnotations if _includeAnnotation(a))
      s.add(a)

  private def _includeFieldOrMethodType(c: Class[_]) =
  schema.fieldMapper.isSupported(c)
  // ! classOf[Query[_]].isAssignableFrom(c)

  @tailrec
  private def _fillWithMembers(clasz: Class[_], members: ArrayBuffer[(Member, HashSet[Annotation])]): Unit = {

    for (
      m <- clasz.getMethods if (m.getDeclaringClass != classOf[Object]) && _includeFieldOrMethodType(m.getReturnType)
    ) {
      m.setAccessible(true)
      val t = (m, new HashSet[Annotation])
      _addAnnotations(m, t._2)
      members.append(t)
    }

    for (m <- clasz.getDeclaredFields if (m.getName.indexOf("$") == -1) && _includeFieldOrMethodType(m.getType)) {
      m.setAccessible(true)
      val t = (m, new HashSet[Annotation])
      _addAnnotations(m, t._2)
      members.append(t)
    }

    val c = clasz.getSuperclass

    if (c != null)
      _fillWithMembers(c, members)
  }
}

object PosoMetaData {
  val finalizeFilter = new CallbackFilter {
    def accept(method: Method): Int =
      if (method.getName == "finalize") 1 else 0
  }
}
