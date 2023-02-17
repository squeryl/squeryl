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
package org.squeryl.dsl.boilerplate

import org.squeryl.internals.{OutMapper, FieldReferenceLinker}
import org.squeryl.dsl.ast.{SelectElement}

class SampleTuple(val outNodes: List[SelectElement], val outMappers: Array[OutMapper[_]]) extends Product {

  override def canEqual(a: Any) = false
  override def equals(a: Any) = false
  override def productArity = outNodes.size
  override def productElement(n: Int): Any = _get(n + 1)

  protected def _get[B](i: Int) = {
    FieldReferenceLinker.putLastAccessedSelectElement(outNodes.apply(i - 1))
    outMappers.apply(i - 1).sample.asInstanceOf[B]
  }
}

object SampleTuple {
  def create(n: List[SelectElement], m: Array[OutMapper[_]]) =
    m.length match {
      case 1 => new STuple1[Any](n, m)
      case 2 => new STuple2[Any, Any](n, m)
      case 3 => new STuple3[Any, Any, Any](n, m)
      case 4 => new STuple4[Any, Any, Any, Any](n, m)
      case 5 => new STuple5[Any, Any, Any, Any, Any](n, m)
      case 6 => new STuple6[Any, Any, Any, Any, Any, Any](n, m)
      case 7 => new STuple7[Any, Any, Any, Any, Any, Any, Any](n, m)
      case 8 => new STuple8[Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 9 => new STuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 10 => new STuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 11 => new STuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 12 => new STuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 13 => new STuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 14 => new STuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 15 => new STuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 16 => new STuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 17 => new STuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 18 =>
        new STuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](n, m)
      case 19 =>
        new STuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any](
          n,
          m
        )
      case 20 =>
        new STuple20[
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any
        ](n, m)
      case 21 =>
        new STuple21[
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any
        ](n, m)
      case 22 =>
        new STuple22[
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any,
          Any
        ](n, m)
      case _ => org.squeryl.internals.Utils.throwError("Tuple23 is not supported")
    }
}

class STuple1[T1](n: List[SelectElement], m: Array[OutMapper[_]]) extends SampleTuple(n, m) {
  def _1 = _get[T1](1)
}

class STuple2[T1, T2](n: List[SelectElement], m: Array[OutMapper[_]]) extends STuple1[T1](n, m) with Product2[T1, T2] {
  def _2 = _get[T2](2)
}

class STuple3[T1, T2, T3](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple2[T1, T2](n, m)
    with Product3[T1, T2, T3] {
  def _3 = _get[T3](3)
}

class STuple4[T1, T2, T3, T4](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple3[T1, T2, T3](n, m)
    with Product4[T1, T2, T3, T4] {
  def _4 = _get[T4](4)
}

class STuple5[T1, T2, T3, T4, T5](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple4[T1, T2, T3, T4](n, m)
    with Product5[T1, T2, T3, T4, T5] {
  def _5 = _get[T5](5)
}

class STuple6[T1, T2, T3, T4, T5, T6](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple5[T1, T2, T3, T4, T5](n, m)
    with Product6[T1, T2, T3, T4, T5, T6] {
  def _6 = _get[T6](6)
}

class STuple7[T1, T2, T3, T4, T5, T6, T7](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple6[T1, T2, T3, T4, T5, T6](n, m)
    with Product7[T1, T2, T3, T4, T5, T6, T7] {
  def _7 = _get[T7](7)
}

class STuple8[T1, T2, T3, T4, T5, T6, T7, T8](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple7[T1, T2, T3, T4, T5, T6, T7](n, m)
    with Product8[T1, T2, T3, T4, T5, T6, T7, T8] {
  def _8 = _get[T8](8)
}

class STuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple8[T1, T2, T3, T4, T5, T6, T7, T8](n, m)
    with Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9] {
  def _9 = _get[T9](9)
}

class STuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](n, m)
    with Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] {
  def _10 = _get[T10](10)
}

class STuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](n, m)
    with Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] {
  def _11 = _get[T11](11)
}

class STuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](n, m)
    with Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] {
  def _12 = _get[T12](12)
}

class STuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](n, m)
    with Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] {
  def _13 = _get[T13](13)
}

class STuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](n, m)
    with Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] {
  def _14 = _get[T14](14)
}

class STuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](n, m)
    with Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] {
  def _15 = _get[T15](15)
}

class STuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](n, m)
    with Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] {
  def _16 = _get[T16](16)
}

class STuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](n, m)
    with Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] {
  def _17 = _get[T17](17)
}

class STuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](n, m)
    with Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] {
  def _18 = _get[T18](18)
}

class STuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](n, m)
    with Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] {
  def _19 = _get[T19](19)
}

class STuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](n, m)
    with Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] {
  def _20 = _get[T20](20)
}

class STuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](n, m)
    with Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] {
  def _21 = _get[T21](21)
}

class STuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
  n: List[SelectElement],
  m: Array[OutMapper[_]]
) extends STuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](n, m)
    with Product22[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
    ] {
  def _22 = _get[T22](22)
}
