package org.squeryl.internals

import scala.quoted._
import scala.quoted.staging._
import scala.quoted.{Quotes, Type}

object Tag {

  // inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  // def inspectAny[T <: AnyKind : Type](using qctx: Quotes): Expr[LightTypeTag] = {

  // given Toolbox = Toolbox.make(getClass.getClassLoader)

  inline def tag[T <: AnyKind]: String = ${ tag[T] }

  def tag[T <: AnyKind: Type](using qctx0: Quotes):Expr[String] = {
    given qctx0.type = qctx0
    import qctx0.reflect.{given, _}
    val tpe = implicitly[Type[T]]
    val uns = TypeTree.of[T]
    val repr = TypeRepr.of[T]
    println("classSymbol: " + repr.classSymbol)
    println("typeSymbol: " + repr.typeSymbol)
    println("termSymbol: " + repr.termSymbol)
    val isProfessor = repr =:= TypeRepr.of[org.squeryl.Professor]
    println("isProfessor: " + isProfessor)
    // I want to use =:= so I think I need the the TypeRepr of the bossId member field

    println("type:" +  tpe)
    println("typetree: " + uns)
    println("repr: " + repr)
    val symbol = uns.symbol
    println("symbol: " + symbol.toString)
    println("fullName: " + symbol.fullName)
    println("declaredFields: " + symbol.declaredFields)
    println("memberFields: " + symbol.memberFields)
    println("memberTypes: " + symbol.memberTypes)
    println("declaredMethods: " + symbol.declaredMethods)
    println("memberMethods: " + symbol.memberMethods)

    // retrieve bossId field Symbol
    val bossIdField: Symbol = uns.symbol.memberField("bossId")
    // println("tpe: " + bossIdField.tpe)
    println("tree: " + bossIdField.tree)
    println("tree.symbol: " + bossIdField.tree.symbol)
    // tree: ValDef(bossId,TypeTree[AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Option),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Long)))],EmptyTree)
    println("isValDef: " + bossIdField.isValDef)
    val bossIdValDef = ValDef(bossIdField, None)
    println("bossIdValDef:" + bossIdValDef)
    println("tpt:" + bossIdValDef.tpt)
    val bossIdType: TypeRepr = bossIdValDef.tpt.tpe
    println("tpe:" + bossIdType)
    println("dealias:" + bossIdType.dealias)
    println("simplified:" + bossIdType.simplified)
    println("classSymbol:" + bossIdType.classSymbol)
    println("typeSymbol:" + bossIdType.typeSymbol)
    println("termSymbol:" + bossIdType.termSymbol)

    // I could go through this to see if it's an Option[Something]
    // TypeTree[AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Option),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scala),class Long)))]

    val isOption = bossIdType.typeSymbol == TypeRepr.of[Option[_]].typeSymbol
    println("isOption: " + isOption)

    val isOptionLong = bossIdType =:= TypeRepr.of[Option[Long]]
    println("isOptionLong: " + isOptionLong)

    val isOptionInt = bossIdType =:= TypeRepr.of[Option[Int]]
    println("isOptionInt: " + isOptionInt)

    // if only I could use TypeRepr.of[T] on the member field

    val bossIdTypeRepr = repr.select(bossIdField) // that didn't do what I thought it would
    println("bossIdTypeRepr: " + bossIdTypeRepr)

    println("baseType: " + repr.baseType(bossIdField))


    // val isOptionLong = bossIdTypeRepr =:= TypeRepr.of[Option[_]]
    // println("isOptionLong: " + isOptionLong)

    val tpe2 = uns.tpe
    println(tpe2)

    // new Inspector(0) { val qctx = qctx0 }.buildTypeRef[T]

    // inspectTType(tpe2)
   //  if (symbol.isNoSymbol)
   //   inspectTType(tpe2)
   // else
   //   inspectSymbol(symbol)
    Expr(tpe.toString)
  }

}
