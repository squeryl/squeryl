package org.squeryl.framework

import org.scalatest.{ Reporter, Stopper, Tracker }
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl.Session

trait RunTestsInsideTransaction extends DbTestBase {

  override def runTest(
    testName: String,
    reporter: Reporter,
    stopper: Stopper,
    configMap: Map[String, Any],
    tracker: Tracker): Unit = {
    if(!notIgnored){
      super.runTest(testName, reporter, stopper, configMap, tracker)      
      return
    }

    // each test occur from within a transaction, that way when the test completes _all_ changes
    // made during the test are reverted so each test gets a clean enviroment to test against
    transaction {
      super.runTest(testName, reporter, stopper, configMap, tracker)

      // we abort the transaction if we get to here, so changes get rolled back
      Session.currentSession.connection.rollback        
    }
  }

}

