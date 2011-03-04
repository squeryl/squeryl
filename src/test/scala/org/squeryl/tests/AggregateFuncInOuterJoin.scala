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

package org.squeryl.tests;

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.JUnitSuiteRunner

@RunWith(classOf[JUnitSuiteRunner])
class LeftJoinTest extends SpecificationWithJUnit {
 import org.squeryl.{ SessionFactory, Session }
 import org.squeryl.PrimitiveTypeMode._
 import org.squeryl.adapters.H2Adapter

 import LeftJoinTest._

 Class.forName("org.h2.Driver")
 SessionFactory.concreteFactory = Some(() => {
   val s = Session.create(java.sql.DriverManager.getConnection("jdbc:h2:~/locktest"), new H2Adapter)
   s.setLogger(println(_))
   s
 })

 transaction {
   drop
   create
   printDdl
   months.insert(new Month(1, "Jan"))
   months.insert(new Month(2, "Feb"))
   months.insert(new Month(3, "Mar"))
   months.insert(new Month(4, "Apr"))
   months.insert(new Month(5, "May"))
   months.insert(new Month(6, "Jun"))
   months.insert(new Month(7, "Jul"))
   months.insert(new Month(8, "Aug"))
   months.insert(new Month(9, "Sep"))
   months.insert(new Month(10, "Oct"))
   months.insert(new Month(11, "Nov"))
   months.insert(new Month(12, "Dec"))

   items.insert(new Item(1, "i1"))

   ordrs.insert(new Ordr(1, 1, 1, 20))
   ordrs.insert(new Ordr(2, 1, 1, 40))
   ordrs.insert(new Ordr(3, 5, 1, 15))
 }

 "Joining to a subquery" should {
   "return the correct results if an inner join is used" in {
     val subquery = from(ordrs)((o) =>
       groupBy(o.monthId)
         compute (sum(o.qty))
         orderBy (o.monthId))

     val mainquery = join(months, subquery)((m, sq) =>
       select(m, sq.measures)
         on (m.id === sq.key))

     val res = transaction { mainquery.toList }

     res.size must beEqualTo(2)
     res(0)._2 must beEqualTo(Some(60))
     res(1)._2 must beEqualTo(Some(15))
   }
   ("return the correct results if a left outer join is used" :ExampleSpecification) in {
     val subquery = from(ordrs)((o) =>
       groupBy(o.monthId)
         compute (sum(o.qty))
         orderBy (o.monthId))

     val mainquery =
       join(months, subquery.leftOuter)((m, sq) =>
         select(m, sq)
         on (m.id === sq.map(_.key))
       )

     val res = transaction {
       mainquery.map(e =>
         if(e._2 == None) None
         else e._2.get.measures
       ).toSeq
     }

     res.size must beEqualTo(12)
     res(0) must beEqualTo(Some(60))
     res(1) must beEqualTo(None)
     res(2) must beEqualTo(None)
     res(3) must beEqualTo(None)
     res(4) must beEqualTo(Some(15))
     res(5) must beEqualTo(None)
     res(6) must beEqualTo(None)
     res(7) must beEqualTo(None)
     res(8) must beEqualTo(None)
     res(9) must beEqualTo(None)
     res(10) must beEqualTo(None)
     res(11) must beEqualTo(None)
   }
 }

}

import org.squeryl.Schema

object LeftJoinTest extends Schema {
 import org.squeryl.PrimitiveTypeMode._

 val items = table[Item]("Item")

 val months = table[Month]("Month")

 val ordrs = table[Ordr]("Ordr")

 override def drop = super.drop
}

class Item(val id: Int, val name: String)

class Month(val id: Int, val name: String) {
 override def toString = "Mont("+id + ":" + name + ")"
}

class Ordr(val id: Int, val monthId: Int, val itemId: Int, val qty: Int)