package org.bitcoins.core.script.bitwise

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/8/16.
  */
class BitwiseOperationsFactoryTest extends BitcoinSUnitTest {

  "BitwiseOperationsFactory" must "match strings with bitwise operations" in {
    BitwiseOperation.fromStringOpt("OP_EQUAL") must be(Some(OP_EQUAL))
    BitwiseOperation.fromStringOpt("OP_EQUALVERIFY") must be(
      Some(OP_EQUALVERIFY))
    BitwiseOperation.fromStringOpt("RANDOM") must be(None)
  }
}
