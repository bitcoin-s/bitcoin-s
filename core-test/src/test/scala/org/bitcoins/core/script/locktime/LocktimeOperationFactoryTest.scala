package org.bitcoins.core.script.locktime

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 1/8/16.
  */
class LocktimeOperationFactoryTest extends BitcoinSUnitTest {

  "LocktimeOperationFactory" must "match lock time operations from strings" in {
    LocktimeOperation.fromStringOpt("OP_CHECKLOCKTIMEVERIFY") must be(
      Some(OP_CHECKLOCKTIMEVERIFY))
  }

}
