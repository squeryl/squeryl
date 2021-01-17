package org.squeryl

import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.staging._

object BorisMain extends App {

  // inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  // def inspectAny[T <: AnyKind : Type](using qctx: Quotes): Expr[LightTypeTag] = {

  // given Toolbox = Toolbox.make(getClass.getClassLoader)

  // inline def tag[T <: AnyKind]: String = ${ tag[T] }
  //
  // def tag[T <: AnyKind: Type](using Quotes):Expr[String] =
  //   val tpe = implicitly[Type[T]]
  //   println(tpe)
  //   Expr("")

  class Professor(val lastName: String, @org.squeryl.annotations.OptionType(classOf[Long]) var bossId: Option[Long]=None)

  def main() =
    // println(Tag.tag[Int])
    // println(Tag.tag[List])
    println(Tag.tag[Option[Int]])
    println(Tag.tag[Professor])

}
