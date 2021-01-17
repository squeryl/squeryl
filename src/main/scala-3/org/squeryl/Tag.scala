package org.squeryl

import scala.quoted._
import scala.quoted.staging._

object Tag {

  // inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  // def inspectAny[T <: AnyKind : Type](using qctx: Quotes): Expr[LightTypeTag] = {

  // given Toolbox = Toolbox.make(getClass.getClassLoader)

  inline def tag[T <: AnyKind]: String = ${ tag[T] }

  def tag[T <: AnyKind: Type](using qctx: Quotes):Expr[String] =
    given qctx.type = qctx
    import qctx.reflect.{given, _}
    val tpe = implicitly[Type[T]]
    val uns = TypeTree.of[T]
    println(tpe)
    println(uns)
    Expr(tpe.toString)

  // def main() =
  //   tag[Int]
  //   tag[List]

}
