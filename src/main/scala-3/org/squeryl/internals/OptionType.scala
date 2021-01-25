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

import java.lang.reflect.Member

protected[internals] object OptionType {

  // FIXME how to implement this in scala3?
  def optionTypeFromScalaSig(member: Member): Option[Class[_]] = None // throw Exception("not implemented in scala 3")

  // def optionTypeFromScalaSig(member: Member): Option[Class[_]] = {
  //   val scalaSigOption = ScalaSigParser.parse(member.getDeclaringClass())
  //   scalaSigOption flatMap { scalaSig =>
  //     val result = scalaSig.symbols.filter { sym =>
  //       member.getName == sym.name
  //     }.collect {
  //       case sym: MethodSymbol => sym.infoType
  //     }.collect {
  //       case tpe: NullaryMethodType => tpe.resultType
  //     }.collect {
  //       case TypeRefType(_, _, Seq(TypeRefType(_, tpe, _))) =>
  //         PartialFunction.condOpt(tpe.name){
  //           case "Int" => classOf[scala.Int]
  //           case "Short" => classOf[scala.Short]
  //           case "Long" => classOf[scala.Long]
  //           case "Double" => classOf[scala.Double]
  //           case "Float" => classOf[scala.Float]
  //           case "Boolean" => classOf[scala.Boolean]
  //           case "Byte" => classOf[scala.Byte]
  //           case "Char" => classOf[scala.Char]
  //         }
  //     }
  //     assert(result.size <= 1)
  //     result.headOption.flatten
  //   }
  // }

}
