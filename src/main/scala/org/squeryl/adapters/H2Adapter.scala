package org.squeryl.adapters

import org.squeryl.internals.{FieldMetaData, DatabaseAdapter}
import org.squeryl.Schema

class H2Adapter extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    var res = "  " + fmd.name + " " + schema._columnTypeFor(fmd, this)
    if(!fmd.isOption)
      res += " not null"

    if(isPrimaryKey)
      res += " primary key"

    if(supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      res += " auto_increment"

    res
  }

}