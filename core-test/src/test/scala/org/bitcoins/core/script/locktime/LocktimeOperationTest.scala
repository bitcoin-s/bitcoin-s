package org.bitcoins.core.script.locktime

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/6/16.
  */
class LocktimeOperationTest extends BitcoinSUnitTest {

  "LocktimeOperations" must "define OP_CHECKLOCKTIMEVERIFY" in {
    OP_CHECKLOCKTIMEVERIFY.opCode must be(177)
  }
}
