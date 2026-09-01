/*******************************************************************************
 * Copyright 2014 Chen Wei <acee06.weichen@gmail.com>
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

import org.squeryl.dsl.ast._
import org.squeryl.internals.StatementWriter

/** Since some distributed databases developed based on MySQL do not support 
  * alias, this adapter is going to adapt such databases. 
  */
class NotSupportAliasDBAdapter extends MySQLInnoDBAdapter {
  override protected def writeQuery(qen: QueryExpressionElements, sw: StatementWriter, 
    inverseOrderBy: Boolean, topHint: Option[String]): Unit = {
    sw.write("Select")

    topHint.foreach(" "  + sw.write(_) + " ")

    if(qen.selectDistinct)
      sw.write(" distinct")
    
    sw.nextLine
    // sw.writeIndented {
    //   sw.writeNodesWithSeparator(qen.selectList.filter(e => ! e.inhibited), ",", true)
    // }
    val selectList = qen.selectList map {
      q => q match {
        case _: FieldSelectElement =>
          val f = q.asInstanceOf[FieldSelectElement]
          f.origin.view.name + "." + f.fieldMetaData.columnName
        case _: TupleSelectElement =>
          val t = q.asInstanceOf[TupleSelectElement]
          t.writeToString.split(" as ").head // support aggregate functions
      }
    } 
    selectList.zipWithIndex foreach {
      case(e, i) =>
      sw.writeIndented(sw.write(e))
      if (i != selectList.size - 1) {
        sw.write(",")
        sw.nextLine
      }
    } 
    sw.nextLine
    sw.write("From")
    sw.nextLine

    if(!qen.isJoinForm) {
      sw.writeIndented {
        for(z <- qen.tableExpressions.zipi) {
          z.element.write(sw)
          if(!z.isLast) {
            sw.write(",")
            sw.nextLine
          }
        }
        sw.pushPendingNextLine
      }
    }
    else {
      val singleNonJoinTableExpression = qen.tableExpressions.filter(! _.isMemberOfJoinList)
      assert(singleNonJoinTableExpression.size == 1, "join query must have exactly one FROM argument, got : " + qen.tableExpressions)
      val firstJoinExpr = singleNonJoinTableExpression.head
      val restOfJoinExpr = qen.tableExpressions.filter(_.isMemberOfJoinList)
      firstJoinExpr.write(sw)
      //sw.write(" ")
      //sw.write(sw.quoteName(firstJoinExpr.alias))
      sw.nextLine

      for(z <- restOfJoinExpr.zipi) {
        writeJoin(z.element, sw)
        if(z.isLast)
          sw.unindent
        sw.pushPendingNextLine
      }
    }

    writeEndOfFromHint(qen, sw)

    if(qen.hasUnInhibitedWhereClause) {      
      sw.write("Where")
      sw.nextLine
      sw.writeIndented {
        qen.whereClause.get.write(sw)
      }
      sw.pushPendingNextLine
    }

    if(! qen.groupByClause.isEmpty) {      
      sw.write("Group By")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.groupByClause.filter(e => ! e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }

    if(! qen.havingClause.isEmpty) {
      sw.write("Having")
      sw.nextLine
      sw.writeIndented {
        sw.writeNodesWithSeparator(qen.havingClause.filter(e => ! e.inhibited), ",", true)
      }
      sw.pushPendingNextLine
    }

    if(! qen.orderByClause.isEmpty && qen.parent == None) {
      sw.write("Order By")
      sw.nextLine
      val ob0 = qen.orderByClause.filter(e => ! e.inhibited)
      val ob = if(inverseOrderBy) ob0.map(_.asInstanceOf[OrderByExpression].inverse) else ob0
      sw.writeIndented {
        sw.writeNodesWithSeparator(ob, ",", true)
      }
      sw.pushPendingNextLine
    }

    writeEndOfQueryHint(qen, sw)

    writePaginatedQueryDeclaration(qen, sw)
    println(sw)
  }

  //override def writeSelectElementAlias(se: SelectElement, sw: StatementWriter) = {}

  override def viewAlias(vn: ViewExpressionNode[_]) = 
    if(vn.view.prefix != None)
      vn.view.prefix.get + "_" + vn.view.name
    else
      vn.view.name

  override def fieldAlias(n: QueryableExpressionNode, fse: FieldSelectElement) = ""
}
