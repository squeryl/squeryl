package org.squeryl.internals

import scala.quoted._
import scala.quoted.staging._
import scala.quoted.{Quotes, Type}

object TypeInfo {

  given ToExpr[Class[_]] with {
    def apply(x: Class[_])(using Quotes) = {
      import quotes.reflect._
      Ref(defn.Predef_classOf).appliedToType(TypeRepr.typeConstructorOf(x)).asExpr.asInstanceOf[Expr[Class[_]]]
    }
  }

  inline def fieldsInfo[T <: AnyKind]: Map[String, Class[_]] = ${ fieldsInfo[T] }

  def fieldsInfo[T <: AnyKind: Type](using qctx0: Quotes): Expr[Map[String, Class[_]]] = {
    given qctx0.type = qctx0
    import qctx0.reflect.{given, _}

    // val uns = TypeTree.of[T]
    // val symbol = uns.symbol
    // val innerClassOfOptionFields: Map[String, Class[Any]] = symbol.memberFields.flatMap { m =>
    //   // we only support val fields for now
    //   if(m.isValDef){
    //     val tpe = ValDef(m, None).tpt.tpe
    //     // only if the field is an Option[_]
    //     if(tpe.typeSymbol == TypeRepr.of[Option[Any]].typeSymbol){
    //       val containedClass: Option[Class[Any]] =
    //         if(tpe =:= TypeRepr.of[Option[Int]]) Some(classOf[Int].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Short]])  Some(classOf[Short].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Long]])  Some(classOf[Long].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Double]])  Some(classOf[Double].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Float]])  Some(classOf[Float].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Boolean]])  Some(classOf[Boolean].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Byte]])  Some(classOf[Byte].asInstanceOf[Class[Any]])
    //         else if(tpe =:= TypeRepr.of[Option[Char]])  Some(classOf[Char].asInstanceOf[Class[Any]])
    //         else None
    //
    //       containedClass.map(clazz => (m.name -> clazz))
    //     } else None
    //   } else None
    // }.toMap

    val result2: Map[String, Class[_]] = Map(
      "bossId" -> classOf[scala.Long],
      "test" -> classOf[scala.Int]
    )

    println(result2)

    Expr(result2)
  }

  // val result2: Map[String, Class[_]] = Map(
  //   "bossId" -> classOf[scala.Long].asInstanceOf[Class[Any]],
  //   "test" -> classOf[scala.Int].asInstanceOf[Class[Any]]
  // )

  // def fieldsInfo[T <: AnyKind: Type](using qctx0: Quotes): Expr[Map[String, Class[_]]] = {
  //   val result = Map("bossId" -> classOf[scala.Long])
  //   Expr(result)
  // }

}

// `Found: (result : Map[String, Class[?]]) Required: Map[String, Class[Any]]`
