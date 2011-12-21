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
package org.squeryl.logging

import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._


object UsageProfileConsolidator {

  def main(args : Array[String]) : Unit =
    if(args.length < 2) {
      printUsage
    }
    else {

      val (dst, src) = args.map(new java.io.File(_)).splitAt(1)

      val notExists = src.filterNot(_.exists)
      if(notExists.size > 0)
        org.squeryl.internals.Utils.throwError("Files don't exist : \n" + notExists.mkString(",\n"))


      Class.forName("org.h2.Driver");

      val dstDb = new Session(
        java.sql.DriverManager.getConnection("jdbc:h2:" + dst.head.getAbsolutePath, "sa", ""),
        new H2Adapter)

      using(dstDb) {
        for(src_i <- src) {

          val srcDb_i = new Session(
            java.sql.DriverManager.getConnection("jdbc:h2:" + src_i.getAbsolutePath, "sa", ""),
            new H2Adapter)

          val (invocations, statements) =
            using(srcDb_i) {
              (StatsSchema.statementInvocations.allRows, StatsSchema.statements.allRows)
            }

          val stmtsToInsert = statements.filter(stmt => StatsSchema.statements.lookup(stmt.id) == None)
          StatsSchema.statements.insert(stmtsToInsert)

          StatsSchema.statementInvocations.insert(invocations)

        }
      }
    }


  def printUsage = {
    println("Usage : ")
    println("java org.squeryl.logging.UsageProfileConsolidator <h2FileForConsolidatedStatsProfile> <list of h2 files to consolidate>")
  }
}