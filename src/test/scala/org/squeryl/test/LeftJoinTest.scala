package org.squeryl.test

import org.squeryl.framework._

abstract class LeftJoinTest extends SchemaTester with RunTestsInsideTransaction{

 import org.squeryl.PrimitiveTypeMode._

  val schema = LeftJoinSchema

 import LeftJoinSchema._

 override def prePopulate {

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

 test("return the correct results if an inner join is used"){
     val subquery = from(ordrs)((o) =>
       groupBy(o.monthId)
         compute (sum(o.qty))
         orderBy (o.monthId))

     val mainquery = join(months, subquery)((m, sq) =>
       select(m, sq.measures)
         on (m.id === sq.key))

     val res = transaction { mainquery.toList }

     res.size should equal(2)
     res(0)._2 should equal(Some(60))
     res(1)._2 should equal(Some(15))
   }
   test("return the correct results if a left outer join is used"){
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

     res.size should equal(12)
     res(0) should equal(Some(60))
     res(1) should equal(None)
     res(2) should equal(None)
     res(3) should equal(None)
     res(4) should equal(Some(15))
     res(5) should equal(None)
     res(6) should equal(None)
     res(7) should equal(None)
     res(8) should equal(None)
     res(9) should equal(None)
     res(10) should equal(None)
     res(11) should equal(None)
   }


}


import org.squeryl.Schema
import org.squeryl.PrimitiveTypeMode._

object LeftJoinSchema extends Schema {

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