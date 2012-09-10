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

class SampleTuple
  (val outNodes: List[SelectElement], val outMappers: Array[OutMapper[_]])
    extends Product {

  override def canEqual(a:Any) = false
  override def equals(a:Any) = false
  override def productArity = outNodes.size
  override def productElement(n: Int):Any = _get(n + 1)

  protected def _get[B](i:Int) = {
    FieldReferenceLinker.putLastAccessedSelectElement(outNodes.apply(i - 1))
    outMappers.apply(i - 1).sample.asInstanceOf[B]
  }
}

object SampleTuple {
  def create(n: List[SelectElement], m: Array[OutMapper[_]]) =
    m.length match {
      case 1 => new STuple1[Any](n,m)
      case 2 => new STuple2[Any,Any](n,m)
      case 3 => new STuple3[Any,Any,Any](n,m)
      case 4 => new STuple4[Any,Any,Any,Any](n,m)
      case 5 => new STuple5[Any,Any,Any,Any,Any](n,m)
      case 6 => new STuple6[Any,Any,Any,Any,Any,Any](n,m)
      case 7 => new STuple7[Any,Any,Any,Any,Any,Any,Any](n,m)
      case 8 => new STuple8[Any,Any,Any,Any,Any,Any,Any,Any](n,m)
      case _ => org.squeryl.internals.Utils.throwError("Tuple9 is not supported, please send a request for supporting up to Product22")
    }
}

class STuple1[T1]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends SampleTuple(n,m) {
  def _1 = _get[T1](1)
}

class STuple2[T1,T2]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple1[T1](n,m) with Product2[T1,T2] {
  def _2 = _get[T2](2)
}

class STuple3[T1,T2,T3]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple2[T1,T2](n,m) with Product3[T1,T2,T3] {
  def _3 = _get[T3](3)
}

class STuple4[T1,T2,T3,T4]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple3[T1,T2,T3](n,m) with Product4[T1,T2,T3,T4] {
  def _4 = _get[T4](4)
}

class STuple5[T1,T2,T3,T4,T5]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple4[T1,T2,T3,T4](n,m) with Product5[T1,T2,T3,T4,T5] {
  def _5 = _get[T5](5)
}

class STuple6[T1,T2,T3,T4,T5,T6]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple5[T1,T2,T3,T4,T5](n,m) with Product6[T1,T2,T3,T4,T5,T6] {
  def _6 = _get[T6](6)
}

class STuple7[T1,T2,T3,T4,T5,T6,T7]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple6[T1,T2,T3,T4,T5,T6](n,m) with Product7[T1,T2,T3,T4,T5,T6,T7] {
  def _7 = _get[T7](7)
}

class STuple8[T1,T2,T3,T4,T5,T6,T7,T8]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple7[T1,T2,T3,T4,T5,T6,T7](n,m) with Product8[T1,T2,T3,T4,T5,T6,T7,T8] {
  def _8 = _get[T8](8)
}
