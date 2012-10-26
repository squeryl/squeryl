package org.squeryl.framework

import org.scalatest.{ Reporter, Stopper, Tracker }
import org.squeryl.PrimitiveTypeMode.transaction
import org.squeryl.{ SessionFactory, Session }

trait RunTestsInsideTransaction extends DbTestBase {
  
  override def withFixture(test: NoArgTest) = {
    super.withFixture(new NoArgTest {
          def name = test.name
          def configMap = test.configMap
          def apply() { 
            if(SessionFactory.concreteFactory.isDefined) {
                //println("Running " + test.name + " in a transaction")
	            transaction {
	            	test.apply()
	            	Session.currentSession.connection.rollback()
	            }
            }
          }
    })
  }

}

