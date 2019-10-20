package org.bitcoins.core.script.bitwise

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/8/16.
  */
class BitwiseOperationsFactoryTest extends BitcoinSUnitTest {

  "BitwiseOperationsFactory" must "match strings with bitwise operations" in {
    BitwiseOperation.fromString("OP_EQUAL") must be(Some(OP_EQUAL))
    BitwiseOperation.fromString("OP_EQUALVERIFY") must be(Some(OP_EQUALVERIFY))
    BitwiseOperation.fromString("RANDOM") must be(None)
  }
}
