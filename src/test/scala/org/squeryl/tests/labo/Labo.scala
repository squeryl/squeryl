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

import org.squeryl.{PrimitiveTypeMode, Query}
import org.squeryl.dsl.ast.{ConstantExpressionNode, TypedExpressionNode}

/**
 *  Ideas to explore :
 *
 *  - Updatable Views ? Could it be used to model inheritance ?
 *
 *  - Dirty check on updates using either
 *    i) intercepter cglib ? (easier on memory harder on CPU)
 *    ii) hash code + signature ? ()
 *
 *  - ConcreteFactory[D <: Driver]
 *    this factory could GCLIb the driver.... what the ?@?#@? ... no need !
 *
 *  - SQL profiling with monitor :
 *      i)   longest running query
 *      ii)  most object producing query
 *      iii) most often called query
 *     all these could point to the code...
 *
 *  Inheritance : !!!!!!***************!*!!!!*
 *    Table[T, S1<:T, S2<:T ...etc] 
 */

object Labo {

//  import PrimitiveTypeMode._
//
//  def vs[T](e: TypedExpressionNode[Scalar,T]) = {}
//  def va[T](e: TypedExpressionNode[Agregate,T]) = {}
//
//
//  val a = new ConstantExpressionNode(1) with AgregateIntOption
//  val s = new ConstantExpressionNode(1) with ScalarInt
//  val ss = new ConstantExpressionNode("a") with ScalarString
//
//
//
//  val s2:ScalarInt = s + 1
////  GroupByz(s + s2)
////  GroupByz(s + 1)
////  GroupByz(2 + s + 1)
//
////  ~:Where(1 === 2) Select(1); //GroupBy(s + s2)
////  ~:Where(1 === 2) Select(1) //.GroupBy(s + 1)
////  ~:Where(1 === 2).Select(1) //.GroupBy(2 + s + 1)
////
////  ~:Where("a" + "q" === "z").Select(1) //.GroupBy(ss)
////
////  ~:Where("a" + "q" === "z").Select(1) //.GroupBy("c")
//
//
//  vs(1)
//  vs(1 + s)
//  vs(s + 1)
//  vs(s + s + 1)
//
//  va(a + 1)
//  va(1 + a)
//  va(1 + s + 1 + a + 3)
//
//  va(max(1 + s))
//  va(s + max(1))
//  va(s + max(s) + 1)
//  va(a + 1 + min(2))
//  va(min(1) + a)
//
//  va(1 * avg(1))
//  va(avg(1) * 1)
//
//  (4 in List(2,3,4) and ("a" in List("s","f"))) : LogicalBoolean
//
//  (4 in List(2,3,4) and "a" === "3") : LogicalBoolean
//
//
//  ("a" in List("s","f")) : LogicalBoolean
//
//  (max(4) in List(2,3,4)) : AgregateLogicalBoolean
//
//  va[Option[String]](min("a"))
//  (min("a") in List("s","f")) : AgregateLogicalBoolean
//
//  ((max(4) in List(2,3,4)) and (min("a") in List("s","f"))) : AgregateLogicalBoolean
//  (1 === min(3)) : AgregateLogicalBoolean
//  ((max(4) in List(2,3,4)) and 1 === min(3)) : AgregateLogicalBoolean
//
//  var x = new ArrayBuffer().appendAll(List(1,4,3,"2"))
//  var y = "allo"
//
//  implicit def t2inerpolater(t: (String,()=>Product)) = new Interpolater(t)
//
//  var r:()=>Product = null
//
//  class Interpolater(var t: (String,()=>Product)) {
//
//    r = t._2
//
//    def interpolate = {
//      println(t._2())
//      val f = t._2.getClass.getFields
//      for(f0 <- f)
//        println("--->"+f0.getName)
//    }
//  }
//
//  def in(p: =>Product) = {
//    val z = p _
//     println(z)
//    z
//  }
//
//  def testI = {
//    val i = ("one is ${x} and allo is ${y}",in(x,y)).interpolate
//    println(i)
//  }
//
//
//
//trait B
//
//trait A {
//  self: B =>
//
//  def z(b: B)
//
//  def b: B = this   // the compiler allows this
//
//  z(this)  // but not this, which is strange,
//
//  z(b) // since this it allowed
//}
//
//  //val a0:A = error("")
//
//  //val b:B = a0
}
