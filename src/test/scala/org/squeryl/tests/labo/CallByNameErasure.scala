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


object CallByNameErasure {

  class B
  class C

  def costlyConstructionOfB: B = {
    error("please wait... cloning the internet")
  }

  def costlyConstructionOfC: C = {
    error("please wait... downloading the internet")
  }

  private implicit val b:B = new B
  private implicit val c:C = new C
  
  def m(i: =>B)(implicit a:B) = println("save the hard work for tomorrow B")
  def m(i: =>C)(implicit b:C) = println("save the hard work for tomorrow C")
  
  def go = {

    m(costlyConstructionOfB)
    m(costlyConstructionOfC)
  }

//  def m(i: ()=>Int) = error("")
//  def m(i: String) = error("")

  //def z(i: Option[Int]) = {}
  //def z(i: Option[Double]) = {}

//  def z1[T <: Int](i: Option[T]) = {}
//  def z1_[T <: Double](i: Option[T]) = {}

//  def m(i: =>B) = error("")
//  def m(i: =>C) = error("")
//  class LazyB(b: ()=>B)
//  class LazyC(c: ()=>C)
//
//  implicit def b2lazyB(b: =>B) = new LazyB(b _)
//  implicit def c2lazyC(c: =>C) = new LazyC(c _)
//  def m(i: LazyB) = println("save the hard work for tomorrow ")
//  def m(i: LazyC) = println("save the hard work for tomorrow ")

}
