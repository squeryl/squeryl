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
  override def productElement(n: Int):Any = _get(n)

  protected def _get[B](i:Int) = {
    FieldReferenceLinker.putLastAccessedSelectElement(outNodes.apply(i - 1))
    outMappers.apply(i - 1).sample.asInstanceOf[B]
  }
}

class STuple1[T1]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends SampleTuple(n,m) {
  def _1 = _get[T1](1)
}

class STuple2[T1,T2]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple1[T1](n,m) {
  def _2 = _get[T2](2)
}

class STuple3[T1,T2,T3]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple2[T1,T2](n,m) {
  def _3 = _get[T3](3)
}

class STuple4[T1,T2,T3,T4]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple3[T1,T2,T3](n,m) {
  def _4 = _get[T4](4)
}

class STuple5[T1,T2,T3,T4,T5]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple4[T1,T2,T3,T4](n,m) {
  def _5 = _get[T5](5)
}

class STuple6[T1,T2,T3,T4,T5,T6]
  (n: List[SelectElement], m: Array[OutMapper[_]])
    extends STuple5[T1,T2,T3,T4,T5](n,m) {
  def _6 = _get[T6](6)
}
