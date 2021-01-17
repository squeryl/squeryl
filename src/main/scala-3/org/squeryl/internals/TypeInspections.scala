package org.squeryl.internals

import scala.quoted.{Quotes, Type}

object TypeInspections {
  def apply[T <: AnyKind : Type](using qctx0: Quotes): Unit = {
    new Inspector(0) { val qctx = qctx0 }.buildTypeRef[T]
  }
  //
  // def nameDb[T <: AnyKind : Type](using qctx0: Quotes): Map[NameReference, Set[NameReference]] = {
  //   new DbInspector(0) { val qctx = qctx0 }.buildNameDb[T]
  // }
  //
  // def fullDb[T <: AnyKind : Type](using qctx0: Quotes): Map[AbstractReference, Set[AbstractReference]] = {
  //   new FullDbInspector(0) { val qctx = qctx0 }.buildFullDb[T]
  // }
}
