/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.adapters

import org.squeryl.{Session, Schema}
import java.sql.{SQLException, ResultSet}
import org.squeryl.internals.{StatementWriter, FieldMetaData, DatabaseAdapter}

class H2Adapter extends DatabaseAdapter {

  override def isFullOuterJoinSupported = false

  override def writeColumnDeclaration(fmd: FieldMetaData, isPrimaryKey: Boolean, schema: Schema): String = {

    var res = "  " + fmd.columnName + " " + databaseTypeFor(fmd)

    for(d <- fmd.defaultValue) {
      val v = convertToJdbcValue(d.value.asInstanceOf[AnyRef])
      if(v.isInstanceOf[String])
        res += " default '" + v + "'"
      else
        res += " default " + v 
    }
    
    if(!fmd.isOption)
      res += " not null"

    if(isPrimaryKey)
      res += " primary key"

    if(supportsAutoIncrementInColumnDeclaration && fmd.isAutoIncremented)
      res += " auto_increment"

    res
  }

  override def isTableDoesNotExistException(e: SQLException): Boolean =
    e.getErrorCode == 42102
}
