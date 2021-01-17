package org.squeryl.internals

import scala.quoted.Quotes

trait InspectorBase {
  // @formatter:off
  val qctx: Quotes
  given qctx.type = qctx
  import qctx.reflect.{given, _}
  // @formatter:on

  protected def shift: Int
  inline val debug = false

  protected def logStart(s: => String): Unit = {
    if (debug) println(" " * shift + s)
  }

  protected def log(s: => String): Unit = {
    if (debug) println(" " * shift + " -> " + s)
  }

  protected def logTpeAttrs[T](uns: => TypeTree): Unit = {
    if (debug) {
      val symbol = uns.symbol
      println(s"Attrs[$uns]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
    }
  }
}
