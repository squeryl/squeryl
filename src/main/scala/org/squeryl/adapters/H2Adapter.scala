package org.squeryl.adapters

import org.squeryl.internals.{FieldMetaData, DatabaseAdapter}

class H2Adapter extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean): String = {

    var res = "  " + fmd.name + " " + databaseTypeFor(fmd.wrappedFieldType)
    if(!fmd.isOption)
      res += " not null"

    if(isPrimaryKey)
      res += " primary key"

    if(areAutoIncrementFieldsSupported && fmd.isAutoIncremented)
      res += " auto_increment"

    res
  }

}