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

package org.squeryl.test.customtypes

import org.squeryl._
import customtypes._
import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.framework.{RunTestsInsideTransaction, QueryTester, SchemaTester}
import org.squeryl.customtypes.CustomTypesMode._
import org.squeryl.annotations._
import reflect.BeanProperty


abstract class DeepNestWithCustomTypes extends SchemaTester with QueryTester with RunTestsInsideTransaction {

  def q1(tp: TipoProducto) =
   from(DeepNestWithCustomTypesSchema.condiciones_ValoresDeCaracteristicas)(
     cv => where(cv.idCondicion === condicion.id and
       cv.idFamiliaProductos === tp.idFamiliaProductos and
       notExists(q2(tp, cv)))
       select (cv)
   )

  def q2(tp: TipoProducto, cv: Condiciones_ValoresDeCaracteristicas) =
   from(DeepNestWithCustomTypesSchema.tiposProducto_valoresDeCaracteristicas)(
     tpv => where(tpv.idTipoProducto === tp.id and
       tpv.caracteristica === cv.caracteristica and
       (tpv.valor notIn(q3(tpv, cv))))
       select(tpv)
   )

  def q3(tpv: TiposProducto_ValoresDeCaracteristicas, cv:
  Condiciones_ValoresDeCaracteristicas) =
   from(DeepNestWithCustomTypesSchema.condiciones_ValoresDeCaracteristicas)(
     cv2 => where(cv2.idCondicion === condicion.id and
       cv2.caracteristica === tpv.caracteristica and
       cv2.idFamiliaProductos === cv.idFamiliaProductos)
       select(cv2.valor))

  test("CrashTest") {
    val q0 =
     from(table)(
       tp =>
         where(exists(q1(tp)))
           select(tp.id.value)
     )

    q0.toList
  }
}

class TiposProducto_ValoresDeCaracteristicas(var idTipoProducto: IntField, var caracteristica: StringField, var valor: StringField)
  extends KeyedEntity[CompositeKey2[IntField, StringField]] {

 def id = compositeKey(idTipoProducto, caracteristica)
 def this() = this(-1, "", "")
}


class TipoProducto(@Column("idTipoProducto") var id: IntField,
                  var idFamiliaProductos: IntField,
                  @BeanProperty var nombre: IntField,
                  @BeanProperty var descripcion: StringField,
                  @BeanProperty var codigo: StringField,
                  //var familiaProductos: FamiliaProductos,
                  @BeanProperty var composicion: StringField,
                  //@BeanProperty var valoresCaracteristicas: List[Caracteristica],
                  @BeanProperty var activo: Boolean)
 extends KeyedEntity[IntField] {

 def this() = this(-1, -1, Nombre.empty, Descripcion.empty, Abreviatura.empty, StringField("SIMPLE"), true)

 override def toString = nombre.toString

 override def equals(other: Any): Boolean = other match {
   case t: TipoProducto => t.id == id
   case _ => false
 }

 def getFamiliaProductos = familiaProductos

// def setFamiliaProductos(fp: FamiliaProductos) {
//   idFamiliaProductos = Option(fp).map(_.id).orNull
//   familiaProductos = fp
// }
}


class Condiciones_ValoresDeCaracteristicas(var idCondicion: IntField, var idFamiliaProductos: IntField, var caracteristica: StringField, var valor: StringField)
  extends KeyedEntity[CompositeKey4[IntField, IntField, StringField, StringField]] {

 def id = compositeKey(idCondicion, idFamiliaProductos, caracteristica, valor)

 def this() = this(-1, -1, "", "")

}

object DeepNestWithCustomTypesSchema extends Schema {

  val condiciones_ValoresDeCaracteristicas =
    table[Condiciones_ValoresDeCaracteristicas]("condiciones_valoresdecaracteristicas")

  val tiposProducto_valoresDeCaracteristicas =
    table[TiposProducto_ValoresDeCaracteristicas]("tiposproducto_valoresdecaracteristicas")

  val tiposProducto =
    table[TipoProducto]("tiposproducto")

  on(tiposProducto)(tp => declare(tp.id is(primaryKey)))
}