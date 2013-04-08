package org.squeryl.test

import org.squeryl._
import org.squeryl.test.schooldb._
import AppSpecificTypeMode._

import org.squeryl.ccast._

object DaSchema extends SchoolDb

object H2Experiments {
  
  import DaSchema._

  
  def main(args: Array[String]): Unit = {
    
    Class.forName("org.h2.Driver")           

    SessionFactory.concreteFactory = Some(() => {
      new Session(
         java.sql.DriverManager.getConnection("jdbc:h2:mem:test_mem"), 
         new adapters.H2Adapter)
    })
    transaction {
      q1
    }
    
  }
  
  
  def q1 = {

    val q = from(from(from(professors)(p0 => select(p0)))(p1 => select(p1)))(p2 => select(p2))    
   
    println(q.ccast.treeString)
    
  }

	  
  implicit class PrettyPrint [ A ] ( a : A ) {
		
	private def indent ( s : String ) = s.lines.toStream match {
	  case h +: t =>
	    ( ("- " + h) +: t.map{"| " + _} ) mkString "\n"
	  case _ => "- "
	}
		
	def treeString : String = a match {
	  case x : Traversable[_] =>
	    x.stringPrefix + ":\n" +
	    x.view
	      .map{ _.treeString }
	      .map{ indent }
	      .mkString("\n")
	  case x : Product if x.productArity == 0 =>
	    x.productPrefix
	  case x : Product =>
	    x.productPrefix + ":\n" +
	    x.productIterator
	      .map{ _.treeString }
	      .map{ indent }
	      .mkString("\n")
	  case null =>
	    "null"
	      case _ =>
	        a.toString
	}
  }  
}
