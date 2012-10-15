package org.squeryl.framework

import org.scalatest.{ Reporter, Stopper, Tracker, Args }
import org.squeryl.PrimitiveTypeMode.transaction
import org.squeryl.Session

trait RunTestsInsideTransaction extends DbTestBase {

  override def runTest(
    testName: String,
    args: Args): Unit = {
    if(!notIgnored){
            
      return
    }

    // each test occur from within a transaction, that way when the test completes _all_ changes
    // made during the test are reverted so each test gets a clean enviroment to test against
    transaction {
      super.runTest(testName, args)

      // we abort the transaction if we get to here, so changes get rolled back
      Session.currentSession.connection.rollback        
    }
  }

}

