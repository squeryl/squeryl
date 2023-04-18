package org.squeryl.internals

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

object TypeInfo {

  inline def fieldsInfo[T <: AnyKind]: Map[String, Class[_]] = ${ fieldsInfo[T] }

  def fieldsInfo[T <: AnyKind: Type](using qctx0: Quotes): Expr[Map[String, Class[_]]] = {
    import qctx0.reflect.*

    val uns = TypeTree.of[T]
    val symbol = uns.symbol
    val innerClassOfOptionFields: Map[String, Class[_]] = symbol.fieldMembers.flatMap { m =>
      // we only support val fields for now
      if (m.isValDef) {
        val tpe = ValDef(m, None).tpt.tpe
        // only if the field is an Option[_]
        if (tpe.typeSymbol == TypeRepr.of[Option[_]].typeSymbol) {
          val containedClass: Option[Class[_]] = PartialFunction.condOpt(tpe.asType) {
            case '[Option[Int]] =>
              classOf[Int]
            case '[Option[Short]] =>
              classOf[Short]
            case '[Option[Long]] =>
              classOf[Long]
            case '[Option[Double]] =>
              classOf[Double]
            case '[Option[Float]] =>
              classOf[Float]
            case '[Option[Boolean]] =>
              classOf[Boolean]
            case '[Option[Byte]] =>
              classOf[Byte]
            case '[Option[Char]] =>
              classOf[Char]
          }

          containedClass.map(clazz => (m.name -> clazz))
        } else None
      } else None
    }.toMap

    Expr(innerClassOfOptionFields)
  }

}
