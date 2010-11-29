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
package org.squeryl.tests

object Labo3 {

  class A {
    def ~ = new A
  }
  class B {
    def ~ = new B
  }

  implicit def int2A(i: Int) = new A  
  implicit def int2B(i: Int) = new B

  def takeA(a: A) = {}
  def takeB(a: B) = {}

//  Here's the thing, a call to
//  1~
//  is clearly not valid, since there is totally  ambiguous,
//  now wouldn't it be great if the type checker could infer
//  that in this context :
//  takeA(1~)
//  the result of 1~ has to be of type A ?
}
