package org.squeryl.internals

import scala.quoted._
import scala.quoted.{Quotes, Type}

object TypeInfo {

  inline def fieldsInfo[T <: AnyKind]: Map[String, Class[_]] = ${ fieldsInfo[T] }

  def fieldsInfo[T <: AnyKind: Type](using qctx0: Quotes): Expr[Map[String, Class[_]]] = {
    given qctx0.type = qctx0
    import qctx0.reflect.{given, _}

    val uns = TypeTree.of[T]
    val symbol = uns.symbol
    val innerClassOfOptionFields: Map[String, Class[_]] = symbol.memberFields.flatMap { m =>
      // we only support val fields for now
      if(m.isValDef){
        val tpe = ValDef(m, None).tpt.tpe
        // only if the field is an Option[_]
        if(tpe.typeSymbol == TypeRepr.of[Option[_]].typeSymbol){
          val containedClass: Option[Class[_]] =
            if(tpe =:= TypeRepr.of[Option[Int]]) Some(classOf[Int])
            else if(tpe =:= TypeRepr.of[Option[Short]])  Some(classOf[Short])
            else if(tpe =:= TypeRepr.of[Option[Long]])  Some(classOf[Long])
            else if(tpe =:= TypeRepr.of[Option[Double]])  Some(classOf[Double])
            else if(tpe =:= TypeRepr.of[Option[Float]])  Some(classOf[Float])
            else if(tpe =:= TypeRepr.of[Option[Boolean]])  Some(classOf[Boolean])
            else if(tpe =:= TypeRepr.of[Option[Byte]])  Some(classOf[Byte])
            else if(tpe =:= TypeRepr.of[Option[Char]])  Some(classOf[Char])
            else None

          containedClass.map(clazz => (m.name -> clazz))
        } else None
      } else None
    }.toMap

    Expr(innerClassOfOptionFields)
  }

}
