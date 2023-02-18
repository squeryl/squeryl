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
import org.squeryl.dsl.ast.SelectElement

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
