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
package org.squeryl.tests.labo

/*

"So now that we have two implicit conversions from Array to ArrayLike values, how does one choose
between them and how does one avoid ambiguities? The trick is to make use of a generalization of over-
loading and implicit resolution in Scala 2.8. Previously, the most specific overloaded method or implicit
conversion would be chosen based solely on the method's argument types. There was an additional clause
which said that the most specific method could not be defined in a proper superclass of any of the other
alternatives. This scheme has been replaced in Scala 2.8 by the following, more liberal one: When com-
paring two different applicable alternatives of an overloaded method or of an implicit, each method gets
one point for having more specific arguments, and another point for being defined in a proper subclass.
An alternative wins over another if it gets a greater number of points in these two comparisons. This
means in particular that if alternatives have identical argument types, the one which is defined in a sub-
class wins.

The sample code below will not compile,
it does works with build r19650-b20091114020153

Information:Compilation completed with 1 error and 0 warnings
Information:1 error
Information:0 warnings

Error:Error:line (22)error: ambiguous reference to overloaded definition,
 both method + in class RInt of type (right: tests.AmbiguousImplicitConv.RDouble)tests.AmbiguousImplicitConv.RDouble
 and  method + in class RInt of type (right: tests.AmbiguousImplicitConv.RInt)tests.AmbiguousImplicitConv.RInt
 match argument types (Int) and expected result type tests.AmbiguousImplicitConv.RType
 takeRType(i + 1)
*/

object AmbiguousImplicitConv {

  class RType

  class RDouble extends RType

  class RInt extends RType {
    //def + (right: RInt) = new RInt
    def + (right: RDouble) = new RDouble
  }

  class RIntSub extends RInt {
    def + (right: RInt) = new RInt
    //def + (right: RDouble) = new RDouble
  }

  implicit def int2RInt(i: Int) = new RInt

  implicit def int2RDouble(d: Double) = new RDouble

  def takeRType(rt: RType) = {}

  val i = new RIntSub

  takeRType(i + 1)
  
/*
  The compiler can't decide between the two conversions :

  i)   i+ int2RInt(1)
  ii)  i+ int2RDouble(Predef.int2double(1))

  I haven't checked in the language spec if, in case of an
  ambiguity, the compiler should choose the shortest path
  (or the lesser count of conversion necessary) to resolve it
  If it's not, then I would put it on my 2.8 wish list ! ;-)
  Anyhow, since it did compile with r19650-b20091114020153,
  I figure it must have been doing this (resolving using the
  "shortest path").
*/
}
