package org.bitcoins.core.script.stack

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 1/8/16.
  */
class StackOperationFactoryTest extends BitcoinSUnitTest {

  "StackOperationFactory" must "match correct operations with their strings" in {
    StackOperation.fromStringOpt("OP_DUP") must be(Some(OP_DUP))
    StackOperation.fromStringOpt("OP_FROMALTSTACK") must be(
      Some(OP_FROMALTSTACK))
    StackOperation.fromStringOpt("RANDOM_OP") must be(None)
    StackOperation.fromStringOpt("OP_IFDUP") must be(Some(OP_IFDUP))
  }
}
